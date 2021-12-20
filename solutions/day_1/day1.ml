open Base
let input =
  Utils.read_puzzle_input 1
    |> String.split_lines
    |> List.map ~f:Int.of_string

let count_descending (measurements: int list) =
  match measurements with
  | [] -> 0
  | _ :: rest ->
    List.zip_exn (List.drop_last_exn measurements) rest
    |> List.count ~f:(fun (a, b) -> a < b)

let solution = 
  (count_descending input, 0)
