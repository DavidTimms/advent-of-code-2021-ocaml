open Base

type alignment = {
  position: int;
  cost: int;
}

let input: int list =
  Utils.read_puzzle_input 7
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:Int.of_string

let cost_to_align_crab (target_position : int) (starting_position : int) : int =
  Int.abs (target_position - starting_position)

let analyse_alignment (initial_positions : int list) (position : int) : alignment =
  {
    position;
    cost = List.sum (module Int) initial_positions ~f:(cost_to_align_crab position)
  }

let find_best_position (initial_positions : int list) : alignment =
  let max = Option.value_exn (List.max_elt initial_positions ~compare:Int.compare) in
  Sequence.range 0 max ~stop:`inclusive
  |> Sequence.map ~f:(analyse_alignment initial_positions)
  |> Sequence.min_elt ~compare:(fun a b -> compare a.cost b.cost)
  |> Option.value_exn

let part1 = (find_best_position input).cost

let part2 = 0
