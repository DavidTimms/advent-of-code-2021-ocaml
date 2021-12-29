open Base

type school = int Sequence.t

let parse_school (school_string : string) : school =
  school_string
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> Sequence.of_list

let input: school =
  Utils.read_puzzle_input 6
  |> parse_school

let count_after_n_days_from_ancestor: int -> int -> int =
  let cache : (int, int) Hashtbl.t = Hashtbl.create (module Int) in
  let rec count_after_n_days_from_ancestor n ancestor_timer =
    match n, ancestor_timer with
    | 0, _ ->
      1
    | _, 0 ->
      (
        match Hashtbl.find cache n with
        | Some count -> count
        | None -> 
          let count =
            count_after_n_days_from_ancestor (n - 1) 6 +
            count_after_n_days_from_ancestor (n - 1) 8
          in
          Hashtbl.set cache ~key:n ~data:count;
          count
      )
    | _, _ ->
      count_after_n_days_from_ancestor (n - 1) (ancestor_timer - 1)
  in count_after_n_days_from_ancestor

let count_after_n_days (n : int) : school -> int =
  Sequence.sum (module Int) ~f:(count_after_n_days_from_ancestor n)

let part1 = input |> count_after_n_days 80

let part2 = input |> count_after_n_days 256
