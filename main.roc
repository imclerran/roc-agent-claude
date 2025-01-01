app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.11.0/z45Wzc-J39TLNweQUoLw3IGZtkQiEN3lTBv3BXErRjQ.tar.br",
}

import pf.Http
import pf.Stdout
import pf.Stderr
import pf.Env
import json.Json
import json.Option exposing [Option]
import Decode exposing [fromBytesPartial]
import pf.Cmd
import pf.File


import "prompt-palindrome.txt" as promptText : Str

cmdOutputFile = "last_cmd_output.txt"

claudeRocFile = "main_claude.roc"

main =
    rocVersionCheck!

    Task.loop { remainingClaudeCalls: 8, prompt: promptText, previousMessages: [] } loopClaude

loopClaude = \{ remainingClaudeCalls, prompt, previousMessages } ->

    info! "Prompt:\n\n$(prompt)\n"

    info! "Asking Claude...\n"
    claudeAnswer = askClaude! prompt previousMessages

    info! "Claude's reply:\n\n$(claudeAnswer)\nEND\n\n"

    codeBlockRes = extractMarkdownCodeBlock claudeAnswer

    when codeBlockRes is
        Ok codeBlock ->
            File.writeUtf8! codeBlock claudeRocFile

            info! "Running `roc check`...\n"
            checkOutputResult = Task.result! executeRocCheck

            stripColorCodes!
            checkOutput = File.readUtf8! cmdOutputFile

            Stdout.line! "\n$(Inspect.toStr checkOutput)\n\n"

            when checkOutputResult is
                Ok {} ->
                    info! "Running `roc test`...\n"
                    testOutputResult = Task.result! executeRocTest

                    stripColorCodes!
                    testOutput = File.readUtf8! cmdOutputFile

                    when testOutputResult is
                        Ok {} ->
                            Stdout.line! "\n$(Inspect.toStr testOutput)\n\n"

                            Task.ok (Done {})
                        
                        Err e ->
                            info! "`roc test` failed.\n"

                            Stderr.line! (Inspect.toStr e)

                            retry remainingClaudeCalls previousMessages prompt claudeAnswer testOutput
                Err e ->
                    info! "`roc check` failed.\n"

                    Stderr.line! (Inspect.toStr e)

                    retry remainingClaudeCalls previousMessages prompt claudeAnswer checkOutput


        Err e ->
            Task.err (ExtractMarkdownCodeBlockFailed (Inspect.toStr e))

retry = \remainingClaudeCalls, previousMessages, oldPrompt, claudeAnswer, newPrompt ->
    if remainingClaudeCalls > 0 then
        newPreviousMessages = List.concat previousMessages [{role: "user", content: oldPrompt}, {role: "assistant", content: claudeAnswer}]

        Task.ok (Step {remainingClaudeCalls: (remainingClaudeCalls - 1), prompt: newPrompt, previousMessages: newPreviousMessages})
    else
        Task.err ReachedMaxClaudeCalls

rocVersionCheck : Task {} _
rocVersionCheck =
    info! "Checking if roc command is available; executing `roc version`:"

    Cmd.exec "roc" ["version"]
    |> Task.mapErr! RocVersionCheckFailed

executeRocCheck =
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "-c"
        |> Cmd.arg """roc check main_claude.roc > last_cmd_output.txt 2>&1"""
    
    Cmd.status bashCmd

executeRocTest =
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "-c"
        |> Cmd.arg """(timeout 2m roc test main_claude.roc > last_cmd_output.txt 2>&1 || { ret=$?; if [ $ret -eq 124 ]; then echo "'roc test' timed out after two minutes!" >> last_cmd_output.txt; fi; exit $ret; })"""
    
    Cmd.status bashCmd

stripColorCodes =
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "removeColorCodes.sh"
    
    Cmd.status bashCmd

extractMarkdownCodeBlock = \text ->
    if !(Str.contains text "```roc") then
        Err (NoRocCodeBlockInClaudeReply text)
    else
        splitOnBackticksRoc = Str.splitOn text "```roc"
        splitOnBackticks =
            List.get? splitOnBackticksRoc 1
            |> Str.splitOn "```"

        when List.get splitOnBackticks 0 is
            Ok codeBlockDirty -> Ok (removeFirstLine codeBlockDirty)
            Err _ -> crash "This should be impossible due to previous if"

removeFirstLine = \str ->
    Str.splitOn str "\n"
    |> List.dropFirst 1
    |> Str.joinWith "\n"


askClaude : Str, List {role: Str, content: Str} -> Task Str _
askClaude = \prompt, previousMessages ->
    escapedPrompt = escapeStr prompt

    escapedPreviousMessages =
        List.map previousMessages \message ->
            { message & content: escapeStr message.content}

    apiKey =
            Env.decode "ANTHROPIC_API_KEY"
                |> Task.mapErr! \_ -> FailedToGetAPIKeyFromEnvVar

    messagesToSend =
        List.append escapedPreviousMessages {role: "user", content: "$(escapedPrompt)"}
        |> messagesToStr

    request = {
        method: Post,
        headers: [
            {key: "x-api-key", value: apiKey},
            {key: "anthropic-version", value: "2023-06-01"},
            {key: "content-type", value: "application/json"},
        ],
        url: "https://api.anthropic.com/v1/messages",
        mimeType: "application/json",
        # models "claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022"
        body: Str.toUtf8
            """
            {
                "model": "claude-3-5-sonnet-20241022",
                "max_tokens": 8192,
                "messages": $(messagesToSend)
            }
            """,
        timeout: TimeoutMilliseconds (5*60*1000),
    }

    sendResult =
        Http.send request
            |> Task.result!

    processedSendResult =
        Result.try sendResult Http.handleStringResponse

    when processedSendResult is
        Ok replyBody ->
            jsonDecoder = Json.utf8With { fieldNameMapping: SnakeCase }

            decoded : DecodeResult ClaudeReply
            decoded = fromBytesPartial (Str.toUtf8 replyBody) jsonDecoder

            when decoded.result is
                Ok claudeReply -> 
                    when List.first claudeReply.content is
                        Ok firstContentElt -> Task.ok firstContentElt.text
                        Err _ -> Task.err ClaudeReplyContentJsonFieldWasEmptyList

                Err e -> Task.err (ClaudeJsonDecodeFailed "Error:\n\tFailed to decode claude API reply into json: $(Inspect.toStr e)\n\n\tbody:\n\t\t$(replyBody)")

        Err err ->
            Task.err (ClaudeHTTPSendFailed err)

messagesToStr : List {role: Str, content: Str} -> Str
messagesToStr = \messages ->
    messagesStr =
        List.map messages \message ->
            """{"role": "$(message.role)", "content": "$(message.content)"}"""
        |> Str.joinWith ", "

    """[$(messagesStr)]"""

ClaudeReply : {
    id : Str,
    type : Str,
    role : Str,
    model : Str,
    content : List { type : Str, text : Str },
    stopReason : Str,
    stopSequence : Option Str,
    usage : {
        inputTokens : U64,
        outputTokens : U64,
    }
}

info = \msg ->
    Stdout.line! "\u(001b)[34mINFO:\u(001b)[0m $(msg)"

escapeStr : Str -> Str
escapeStr = \str ->
    Str.replaceEach str "\\" "\\\\"
    |> Str.replaceEach "\n" "\\n"
    |> Str.replaceEach "\t" "\\t"
    |> Str.replaceEach "\"" "\\\""

    