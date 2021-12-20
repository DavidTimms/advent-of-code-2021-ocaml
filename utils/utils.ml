
let read_puzzle_input ?(example=false) day =
  let path =
    "./inputs/day" ^
    (string_of_int day) ^
    (if example then "_example" else "") ^
    ".txt"
  in
  let channel = open_in path in
  Stdio.In_channel.input_all channel
