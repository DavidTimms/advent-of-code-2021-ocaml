open Base

let binary_string_to_int_list s =
  s |> String.to_list
    |> List.map ~f:(fun digit ->
      match digit with
      | '0' -> 0
      | '1' -> 1
      | _ -> raise (Failure "A binary string must only contain zeros and ones")
    )

let find_length_and_count_per_postion report =
  let bit_length = List.length (List.hd_exn report) in
  let init = (0, List.init bit_length ~f:(Fn.const 0)) in
  List.fold report ~init ~f:(fun (length, totals) digits -> 
      (length + 1, List.map2_exn totals digits ~f:(+))
  )

let find_gamma_and_epsilon report =
  let (length, count_per_position) = find_length_and_count_per_postion report in
  List.fold count_per_position ~init:(0, 0) ~f:(fun (gamma, epsilon) count -> 
    if count <= length / 2 then
      (1 + Int.shift_left gamma 1, Int.shift_left epsilon 1)
    else
      (Int.shift_left gamma 1, 1 + Int.shift_left epsilon 1)
  )

let digits_to_int =
  List.fold ~init:0 ~f:(fun current digit -> digit + Int.shift_left current 1)

let partition_by_nth_digit n report =
  List.partition_tf report ~f:(fun digits -> (List.nth_exn digits n) = 1)

let rec o2_generator_rating ratings n =
  match ratings with
  | [] -> raise (Failure "Unable to find oxygen generator rating")
  | rating :: [] -> digits_to_int rating
  | _ ->
    let (ones, zeros) = partition_by_nth_digit n ratings in
    if List.length ones >= List.length zeros then
      o2_generator_rating ones (n + 1)
    else
      o2_generator_rating zeros (n + 1)

let rec co2_scrubber_rating ratings n =
  match ratings with
  | [] -> raise (Failure "Unable to find CO2 scrubber rating")
  | rating :: [] -> digits_to_int rating
  | _ ->
    let (ones, zeros) = partition_by_nth_digit n ratings in
    if List.length ones < List.length zeros then
      co2_scrubber_rating ones (n + 1)
    else
      co2_scrubber_rating zeros (n + 1)

let input =
  Utils.read_puzzle_input 3
  |> String.split_lines
  |> List.map ~f:binary_string_to_int_list

let part1 =
  let (gamma, epsilon) = find_gamma_and_epsilon input in
  gamma * epsilon

let part2 =
  o2_generator_rating input 0 * co2_scrubber_rating input 0
