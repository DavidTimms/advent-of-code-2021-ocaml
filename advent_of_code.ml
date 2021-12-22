
let solution_to_string (part1, part2) =
  Printf.sprintf "Part 1 = %i, Part 2 = %i" part1 part2

let () = 
  let chosen_day = int_of_string (Array.get Sys.argv 1) in
  print_endline (
    match chosen_day with
    | 1 -> solution_to_string (Day1.part1, Day1.part2)
    | 2 -> solution_to_string (Day2.part1, Day2.part2)
    | 3 -> solution_to_string (Day3.part1, Day3.part2)
    | 4 -> solution_to_string (Day4.part1, Day4.part2)
    | _ -> "Puzzle not completed yet"
  )
