sed -i.bak -E "s/\x1B\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]//g" last_cmd_output.txt && rm last_cmd_output.txt.bak
