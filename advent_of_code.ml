
let solution_to_string (part1, part2) =
  Printf.sprintf "Part 1 = %i, Part 2 = %i" part1 part2

let () = 
  let chosen_day = int_of_string (Array.get Sys.argv 1) in
  print_endline (match chosen_day with
    | 1 -> solution_to_string Day1.solution
    | _ -> "Puzzle not completed yet"
  )
