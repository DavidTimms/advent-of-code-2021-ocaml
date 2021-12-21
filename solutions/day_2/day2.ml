open Base

type command =
  | Forward of int
  | Up of int
  | Down of int

  let parse_command line =
    let (direction, distance_string) = String.rsplit2_exn line ~on:' ' in
    let distance = Int.of_string distance_string in
    match direction with
    | "forward" -> Forward distance
    | "up" -> Up distance
    | "down" -> Down distance
    | _ -> raise (Failure ("Invalid command: " ^ line)) 

let input =
  Utils.read_puzzle_input 2
    |> String.split_lines
    |> List.map ~f:parse_command

let perform_command (x, y) command =
  match command with
  | Forward distance -> (x, y + distance)
  | Up distance -> (x - distance, y)
  | Down distance -> (x + distance, y)

let perform_commands =
  List.fold ~init:(0, 0) ~f:perform_command

let perform_command_with_aim (x, y, aim) command =
  match command with
  | Forward distance -> (x + distance, y + (distance * aim), aim)
  | Up amount -> (x, y, aim - amount)
  | Down amount -> (x, y, aim + amount)

let perform_commands_with_aim =
  List.fold ~init:(0, 0, 0) ~f:perform_command_with_aim

let part1 = perform_commands input |> fun (x, y) -> x * y

let part2 = perform_commands_with_aim input |> fun (x, y, _) -> x * y
