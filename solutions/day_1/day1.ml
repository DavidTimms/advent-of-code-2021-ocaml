open Base

let input =
  Utils.read_puzzle_input 1
    |> String.split_lines
    |> List.map ~f:Int.of_string

let count_increasing (measurements: int list) =
  match measurements with
  | [] -> 0
  | _ :: rest ->
    List.zip_exn (List.drop_last_exn measurements) rest
    |> List.count ~f:(fun (a, b) -> a < b)

let drop_last_n n values =
  let rec drop_last_n_either n values =
    match values with
    | [] -> Either.First 0
    | first :: rest ->
      match drop_last_n_either n rest with
      | Either.First distance_to_tail ->
        if distance_to_tail < n then
          Either.First (distance_to_tail + 1)
        else
          Either.Second [first]
      | Either.Second remaining ->
        Either.Second (first :: remaining)
  in
  match drop_last_n_either n values with
  | Either.First _ -> []
  | Either.Second remaining -> remaining

let rec sum_sliding_window n values =
  if n = 1 then
    values
  else
    match values with
    | [] -> []
    | _ :: rest ->
      List.map2_exn ~f:(+) (drop_last_n (n - 1) values) (sum_sliding_window (n - 1) rest)

let part1 = count_increasing input

let part2 = sum_sliding_window 3 input |> count_increasing
