app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.12.0/1trwx8sltQ-e9Y2rOB4LWUWLS_sFVyETK8Twl0i9qpw.tar.gz",
    ai: "https://github.com/imclerran/roc-ai/releases/download/v0.9.0/LIMYIvGIjaL4cbOXI6mgjV5pQEdNVE5w_G8ggz1uxGU.tar.br",
}

import pf.Http
import pf.Stdout
import pf.Stderr
import pf.Env
import pf.Cmd
import pf.File
import ai.Chat
import ai.Client exposing [Client]

import "prompt-palindrome.txt" as prompt_text : Str

# Output of `roc test` and `roc check` gets written to this file
cmd_output_file = "last_cmd_output.txt"

# Claude will write to this file and execute `roc check` and `roc test` on it
claude_roc_file = "main_claude.roc"
claude_max_requests = 8
# Choose between:
# - smartest, expensive: "claude-3-5-sonnet-20241022"
# - decent, cheap, fast: "claude-3-5-haiku-20241022"
claude_model = "claude-3-5-sonnet-20241022"

# http_request_timeout = 5 * 60 * 1000

main! = |_args|
    try roc_version_check!({})

    api_key = Env.var!("ANTHROPIC_API_KEY")?

    client = Client.new({ api: Anthropic, model: claude_model, api_key, max_tokens: 8192 })

    try loop_claude!(claude_max_requests, prompt_text, client)

    Ok({})

loop_claude! = |remaining_claude_calls, prompt, client|

    try info!("Prompt:\n\n${prompt}\n")

    try info!("Asking Claude...\n")
    with_claude_answer = try ask_claude!(prompt, client)

    claude_answer = List.last(with_claude_answer.messages) |> Result.map_ok(.content)?

    try info!("Claude's reply:\n\n${claude_answer}\nEND\n\n")

    code_block_res = extract_markdown_code_block(claude_answer)

    when code_block_res is
        Ok(code_block) ->
            try File.write_utf8!(code_block, claude_roc_file)

            try info!("Running `roc check`...\n")
            check_output_result = execute_roc_check!({})

            try strip_color_codes!({})
            check_output = try File.read_utf8!(cmd_output_file)

            try Stdout.line!("\n${Inspect.to_str(check_output)}\n\n")

            when check_output_result is
                Ok({}) ->
                    try info!("Running `roc test`...\n")
                    test_output_result = execute_roc_test!({})

                    try strip_color_codes!({})
                    test_output = try File.read_utf8!(cmd_output_file)

                    when test_output_result is
                        Ok({}) ->
                            try Stdout.line!("\n${Inspect.to_str(test_output)}\n\n")

                            Ok({})

                        Err(e) ->
                            try info!("`roc test` failed.\n")

                            try Stderr.line!(Inspect.to_str(e))

                            retry!(remaining_claude_calls, client, test_output)

                Err(e) ->
                    try info!("`roc check` failed.\n")

                    try Stderr.line!(Inspect.to_str(e))

                    retry!(remaining_claude_calls, client, check_output)

        Err(e) ->
            Err(ExtractMarkdownCodeBlockFailed(Inspect.to_str(e)))

ask_claude! : Str, Client => Result Client _
ask_claude! = |prompt, client|
    escaped_prompt = escape_str(prompt)
    with_prompt = Chat.append_user_message(client, escaped_prompt, {})
    request = Chat.build_http_request(with_prompt, {})

    response =
        Http.send!(request)?

    Chat.update_messages(client, response) 
        |> Result.map_err(|err|
            when err is
                NoChoices -> ClaudeReplyContentJsonFieldWasEmptyList
                BadJson str -> ClaudeJsonDecodeFailed("Error:\n\tFailed to decode claude API reply into json: {Inspect.to_str(e)}\n\n\tbody:\n\t\t${str}")
                DecodingError -> ClaudeJsonDecodeFailed("Error:\n\tFailed to decode claude API reply into json: {Inspect.to_str(e)}\n\n\tbody:\n\t{response.body}")
                HttpError e -> ClaudeHTTPSendFailed(e)
                ApiError e -> ClaudeApiRquestFailed(e)
    )

retry! = |remaining_claude_calls, client, new_prompt|
    if remaining_claude_calls > 0 then
        loop_claude!((remaining_claude_calls - 1), new_prompt, client)
    else
        Err(ReachedMaxClaudeCalls)

execute_roc_check! = |{}|
    bash_cmd =
        Cmd.new("bash")
        |> Cmd.arg("-c")
        |> Cmd.arg("roc check main_claude.roc > last_cmd_output.txt 2>&1")

    cmd_exit_code = try Cmd.status!(bash_cmd)

    if cmd_exit_code != 0 then
        Err(StripColorCodesFailedWithExitCode(cmd_exit_code))
    else
        Ok({})

execute_roc_test! = |{}|
    bash_cmd =
        Cmd.new("bash")
        |> Cmd.arg("-c")
        |> Cmd.arg(
            """
            (timeout 2m roc test main_claude.roc > last_cmd_output.txt 2>&1 || { ret=$?; if [ $ret -eq 124 ]; then echo "'roc test' timed out after two minutes!" >> last_cmd_output.txt; fi; exit $ret; })
            """,
        )

    cmd_exit_code = try Cmd.status!(bash_cmd)

    if cmd_exit_code != 0 then
        Err(StripColorCodesFailedWithExitCode(cmd_exit_code))
    else
        Ok({})

# HELPERS

roc_version_check! : {} => Result {} _
roc_version_check! = |{}|
    try info!("Checking if roc command is available; executing `roc version`:")

    Cmd.exec!("roc", ["version"])
    |> Result.map_err(RocVersionCheckFailed)

strip_color_codes! = |{}|
    bash_cmd =
        Cmd.new("bash")
        |> Cmd.arg("removeColorCodes.sh")

    cmd_exit_code = try Cmd.status!(bash_cmd)

    if cmd_exit_code != 0 then
        Err(StripColorCodesFailedWithExitCode(cmd_exit_code))
    else
        Ok({})

extract_markdown_code_block = |text|
    if !(Str.contains(text, "```roc")) then
        Err(NoRocCodeBlockInClaudeReply(text))
    else
        split_on_backticks_roc = Str.split_on(text, "```roc")
        split_on_backticks =
            List.get(split_on_backticks_roc, 1)?
            |> Str.split_on("```")

        when List.get(split_on_backticks, 0) is
            Ok(code_block_dirty) -> Ok(remove_first_line(code_block_dirty))
            Err(_) -> crash("This should be impossible due to previous if")

remove_first_line = |str|
    Str.split_on(str, "\n")
    |> List.drop_first(1)
    |> Str.join_with("\n")

info! = |msg|
    Stdout.line!("\u(001b)[34mINFO:\u(001b)[0m ${msg}")

escape_str : Str -> Str
escape_str = |str|
    Str.replace_each(str, "\\", "\\\\")
    |> Str.replace_each("\n", "\\n")
    |> Str.replace_each("\t", "\\t")
    |> Str.replace_each("\"", "\\\"")

