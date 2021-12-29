open Base

type alignment = {
  position: int;
  cost: int;
}

type fuel_usage =
  | Flat
  | Increasing

let input: int list =
  Utils.read_puzzle_input 7
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:Int.of_string

let rec cost_to_move_crab ~fuel_usage (distance : int): int =
  match fuel_usage with
  | Flat -> distance
  | Increasing -> 
    if distance = 0 then
      0
    else
      distance + cost_to_move_crab (distance - 1) ~fuel_usage

let cost_to_align_crab ~fuel_usage (target_position : int) (starting_position : int) : int =
  let distance = Int.abs (target_position - starting_position) in
  cost_to_move_crab distance ~fuel_usage

let analyse_alignment ~fuel_usage (initial_positions : int list) (position : int) : alignment =
  {
    position;
    cost = List.sum (module Int) initial_positions ~f:(cost_to_align_crab position ~fuel_usage)
  }

let find_best_position ~fuel_usage (initial_positions : int list) : alignment =
  let max = Option.value_exn (List.max_elt initial_positions ~compare:Int.compare) in
  Sequence.range 0 max ~stop:`inclusive
  |> Sequence.map ~f:(analyse_alignment initial_positions ~fuel_usage)
  |> Sequence.min_elt ~compare:(fun a b -> compare a.cost b.cost)
  |> Option.value_exn

let part1 = (find_best_position input ~fuel_usage:Flat).cost

let part2 = (find_best_position input ~fuel_usage:Increasing).cost
