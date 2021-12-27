open Base

module Point = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) =
      let x_compared = Int.compare x1 x2 in
      if x_compared <> 0 then
        x_compared
      else
        Int.compare y1 y2

    let sexp_of_t (x, y) : Sexp.t =
      List [Atom (Int.to_string x); Atom (Int.to_string y)]
  
  end

include T
include Comparator.Make(T)
end

type vent_line = {
  start: Point.t;
  finish: Point.t;
}

type point_frequency_map = (Point.t, int, Point.comparator_witness) Map.t

(*
let tap f value =
  f value;
  value

let print_point (x, y) =
  Stdio.printf "(%i,%i)\n" x y

let print_vent_line { start = (x1, y1); finish = (x2, y2) } =
  Stdio.printf "(%i,%i) -> (%i,%i)\n" x1 y1 x2 y2
*)

let parse_vent_line coordinates_string =
  let regex = Str.regexp "^\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)$" in
  if Str.string_match regex coordinates_string 0 then
    let group n = Int.of_string (Str.matched_group n coordinates_string) in
    {
      start = (group 1, group 2); 
      finish = (group 3, group 4);
    }
  else
    raise (Failure ("Invalid line: " ^ coordinates_string))

let input =
  Utils.read_puzzle_input 5
  |> String.split_lines
  |> List.map ~f:parse_vent_line

let is_line_diagonal { start = (x1, y1); finish = (x2, y2) } =
  x1 <> x2 && y1 <> y2

let ignore_diagonal_lines =
  List.filter ~f:(Fn.non is_line_diagonal)

let line_to_points { start = (x1, y1); finish = (x2, y2) } =
  let length = Int.max (Int.abs (x1 - x2)) (Int.abs (y1 - y2)) in
  let x_direction = Int.compare x2 x1 in
  let y_direction = Int.compare y2 y1 in
  List.range ~start:`inclusive ~stop:`inclusive 0 length
  |> List.map ~f:(fun i -> (x1 + (i * x_direction), y1 + (i * y_direction)))

let count_points: Point.t list -> point_frequency_map =
    List.fold ~init:(Map.empty (module Point)) ~f:(fun frequency_map point ->
      Map.update frequency_map point ~f:(function
        | Some(count) -> count + 1
        | None -> 1
      )
    )

let find_most_dangerous_points lines =
  lines
  |> List.concat_map ~f:line_to_points
  |> count_points
  |> Map.filter ~f:(fun count -> count > 1)
  |> Map.keys

let part1 =
  input
  |> ignore_diagonal_lines
  |> find_most_dangerous_points
  |> List.length

let part2 =
  input
  |> find_most_dangerous_points
  |> List.length
