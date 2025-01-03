app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br",
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

# Output of `roc test` and `roc check` gets written to this file
cmdOutputFile = "last_cmd_output.txt"

# Claude will write to this file and execute `roc check` and `roc test` on it
claudeRocFile = "main_claude.roc"
claudeMaxRequests = 8
# Choose between:
# - smartest, expensive: "claude-3-5-sonnet-20241022"
# - decent, cheap, fast: "claude-3-5-haiku-20241022"
claudeModel = "claude-3-5-sonnet-20241022"

httpRequestTimeout = 5*60*1000

main! = \_args ->
    try rocVersionCheck! {}

    try loopClaude! claudeMaxRequests promptText []

    Ok {}

loopClaude! = \remainingClaudeCalls, prompt, previousMessages ->

    try info! "Prompt:\n\n$(prompt)\n"

    try info! "Asking Claude...\n"
    claudeAnswer = try askClaude! prompt previousMessages

    try info! "Claude's reply:\n\n$(claudeAnswer)\nEND\n\n"

    codeBlockRes = extractMarkdownCodeBlock claudeAnswer

    when codeBlockRes is
        Ok codeBlock ->
            try File.write_utf8! codeBlock claudeRocFile

            try info! "Running `roc check`...\n"
            checkOutputResult = executeRocCheck! {}

            try stripColorCodes! {}
            checkOutput = try File.read_utf8! cmdOutputFile

            try Stdout.line! "\n$(Inspect.toStr checkOutput)\n\n"

            when checkOutputResult is
                Ok {} ->
                    try info! "Running `roc test`...\n"
                    testOutputResult = executeRocTest! {}

                    try stripColorCodes! {}
                    testOutput = try File.read_utf8! cmdOutputFile

                    when testOutputResult is
                        Ok {} ->
                            try Stdout.line! "\n$(Inspect.toStr testOutput)\n\n"

                            Ok {}
                        
                        Err e ->
                            try info! "`roc test` failed.\n"

                            try Stderr.line! (Inspect.toStr e)

                            retry! remainingClaudeCalls previousMessages prompt claudeAnswer testOutput
                Err e ->
                    try info! "`roc check` failed.\n"

                    try Stderr.line! (Inspect.toStr e)

                    retry! remainingClaudeCalls previousMessages prompt claudeAnswer checkOutput


        Err e ->
            Err (ExtractMarkdownCodeBlockFailed (Inspect.toStr e))

askClaude! : Str, List {role: Str, content: Str} => Result Str _
askClaude! = \prompt, previousMessages ->
    escapedPrompt = escapeStr prompt

    escapedPreviousMessages =
        List.map previousMessages \message ->
            { message & content: escapeStr message.content}

    apiKey =
            Env.decode! "ANTHROPIC_API_KEY"
            |> Result.mapErr \_ -> FailedToGetAPIKeyFromEnvVar
            |> try

    messagesToSend =
        List.append escapedPreviousMessages {role: "user", content: "$(escapedPrompt)"}
        |> messagesToStr

    request = {
        method: Post,
        headers: [
            {name: "x-api-key", value: apiKey},
            {name: "anthropic-version", value: "2023-06-01"},
            {name: "content-type", value: "application/json"},
        ],
        uri: "https://api.anthropic.com/v1/messages",
        body: Str.toUtf8
            """
            {
                "model": "$(claudeModel)",
                "max_tokens": 8192,
                "messages": $(messagesToSend)
            }
            """,
        timeout_ms: TimeoutMilliseconds httpRequestTimeout,
    }

    response =
        Http.send! request

    responseBody =
        Str.fromUtf8 response.body

    when responseBody is
        Ok replyBody ->
            jsonDecoder = Json.utf8With { fieldNameMapping: SnakeCase }

            decoded : DecodeResult ClaudeReply
            decoded = fromBytesPartial (Str.toUtf8 replyBody) jsonDecoder

            when decoded.result is
                Ok claudeReply -> 
                    when List.first claudeReply.content is
                        Ok firstContentElt -> Ok firstContentElt.text
                        Err _ -> Err ClaudeReplyContentJsonFieldWasEmptyList

                Err e -> Err (ClaudeJsonDecodeFailed "Error:\n\tFailed to decode claude API reply into json: $(Inspect.toStr e)\n\n\tbody:\n\t\t$(replyBody)")

        Err err ->
            Err (ClaudeHTTPSendFailed err)

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

retry! = \remainingClaudeCalls, previousMessages, oldPrompt, claudeAnswer, newPrompt ->
    if remainingClaudeCalls > 0 then
        newPreviousMessages = List.concat previousMessages [{role: "user", content: oldPrompt}, {role: "assistant", content: claudeAnswer}]

        loopClaude! (remainingClaudeCalls - 1) newPrompt newPreviousMessages
    else
        Err ReachedMaxClaudeCalls

executeRocCheck! = \{} ->
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "-c"
        |> Cmd.arg """roc check main_claude.roc > last_cmd_output.txt 2>&1"""
    
    cmd_exit_code = try Cmd.status! bashCmd

    if cmd_exit_code != 0 then
        Err (StripColorCodesFailedWithExitCode cmd_exit_code)
    else
        Ok {}

executeRocTest! = \{} ->
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "-c"
        |> Cmd.arg """(timeout 2m roc test main_claude.roc > last_cmd_output.txt 2>&1 || { ret=$?; if [ $ret -eq 124 ]; then echo "'roc test' timed out after two minutes!" >> last_cmd_output.txt; fi; exit $ret; })"""
    
    cmd_exit_code = try Cmd.status! bashCmd

    if cmd_exit_code != 0 then
        Err (StripColorCodesFailedWithExitCode cmd_exit_code)
    else
        Ok {}


# HELPERS

rocVersionCheck! : {} => Result {} _
rocVersionCheck! = \{} ->
    try info! "Checking if roc command is available; executing `roc version`:" 

    Cmd.exec! "roc" ["version"]
    |> Result.mapErr RocVersionCheckFailed

stripColorCodes! = \{} ->
    bashCmd =
        Cmd.new "bash"
        |> Cmd.arg "removeColorCodes.sh"
    
    cmd_exit_code = try Cmd.status! bashCmd

    if cmd_exit_code != 0 then
        Err (StripColorCodesFailedWithExitCode cmd_exit_code)
    else
        Ok {}

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

messagesToStr : List {role: Str, content: Str} -> Str
messagesToStr = \messages ->
    messagesStr =
        List.map messages \message ->
            """{"role": "$(message.role)", "content": "$(message.content)"}"""
        |> Str.joinWith ", "

    """[$(messagesStr)]"""


info! = \msg ->
    Stdout.line! "\u(001b)[34mINFO:\u(001b)[0m $(msg)"

escapeStr : Str -> Str
escapeStr = \str ->
    Str.replaceEach str "\\" "\\\\"
    |> Str.replaceEach "\n" "\\n"
    |> Str.replaceEach "\t" "\\t"
    |> Str.replaceEach "\"" "\\\""

    