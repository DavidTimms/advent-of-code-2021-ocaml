open Base

type board = int list list

type bingo_game = {
  drawn_numbers: int list;
  boards: board list;
}

let parse_drawn_numbers line =
  String.split line ~on:',' |> List.map ~f:Int.of_string

let parse_board_row line =
  String.split line ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:Int.of_string

let parse_boards lines =
  List.fold lines ~init:([]: board list) ~f:(fun boards line ->
    if String.is_empty line then
      [] :: boards
    else
      let (current_board, previous_boards) =
        match boards with
        | [] -> ([], [])
        | head :: tail -> (head, tail)
      in
      ((parse_board_row line) :: current_board) :: previous_boards
  )


let parse_game input =
  match String.split_lines input with
  | [] -> raise (Failure "Invalid puzzle input")
  | first_line :: other_lines ->
    {
      drawn_numbers = parse_drawn_numbers first_line;
      boards = parse_boards other_lines;
    }

let input =
  parse_game (Utils.read_puzzle_input 4 ~example:true)

let part1 = List.length @@ List.hd_exn @@ List.hd_exn @@ List.tl_exn input.boards

let part2 = 0