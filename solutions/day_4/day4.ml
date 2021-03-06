open Base

type board = int list list

type bingo_game = {
  drawn_numbers: int list;
  boards: board list;
}

let empty_int_set =
  Set.empty (module Int)

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
  parse_game (Utils.read_puzzle_input 4)

let board_has_won drawn_numbers board =
  let has_been_drawn = Set.mem drawn_numbers in
  let (incomplete_rows, incomplete_cols) =
    List.foldi board ~init:(empty_int_set, empty_int_set) ~f:(fun row init ->
      List.foldi ~init ~f:(fun col (incomplete_rows, incomplete_cols) number ->
        if has_been_drawn number then
          (incomplete_rows, incomplete_cols)
        else
          (Set.add incomplete_rows row, Set.add incomplete_cols col)
      )
    )
  in
  Set.length incomplete_rows < 5 || Set.length incomplete_cols < 5


let find_winning_board drawn_numbers: board list -> board option =
  List.find ~f:(board_has_won drawn_numbers)

let remove_completed_boards drawn_numbers: board list -> board list =
  List.filter ~f:(Fn.non (board_has_won drawn_numbers))

let calculate_score drawn_numbers final_number board =
  let is_unmarked = Fn.non (Set.mem drawn_numbers) in
  let sum_of_unmarked_numbers =
    List.concat board
    |> List.filter ~f:is_unmarked
    |> List.sum (module Int) ~f:Fn.id
  in
  sum_of_unmarked_numbers * final_number

let find_winning_score game =
  let open Continue_or_stop in
  List.fold_until game.drawn_numbers
    ~init:empty_int_set
    ~f:(fun previous_numbers drawn_number ->
      let drawn_numbers = Set.add previous_numbers drawn_number in
      match find_winning_board drawn_numbers game.boards with
      | None -> Continue drawn_numbers
      | Some board -> Stop (calculate_score drawn_numbers drawn_number board)
    )
    ~finish:(fun _ -> raise (Failure "No winner found"))

let find_losing_score game =
  let open Continue_or_stop in
  List.fold_until game.drawn_numbers
    ~init:(empty_int_set, game.boards)
    ~f:(fun (previous_numbers, previous_remaining_boards) drawn_number ->
      let drawn_numbers =
        Set.add previous_numbers drawn_number in
      let remaining_boards =
        remove_completed_boards drawn_numbers previous_remaining_boards in
      if List.is_empty remaining_boards then
        let losing_board = List.hd_exn previous_remaining_boards in
        Stop (calculate_score drawn_numbers drawn_number losing_board)
      else
        Continue (drawn_numbers, remaining_boards)
    )
    ~finish:(fun _ -> raise (Failure "Some boards will never win"))
let part1 = find_winning_score input

let part2 = find_losing_score input
