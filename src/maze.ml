open! Core

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let of_file input_file : char array array =
  let input_lists = In_channel.read_lines (File_path.to_string input_file) in
  Array.of_list
    (List.map input_lists ~f:(fun line ->
       Array.of_list (String.to_list line)))
;;

let find char (array : char array array) : int * int =
  let char_x = ref 0 in
  let char_y = ref 0 in
  Array.iteri
    ~f:(fun y list ->
      Array.iteri
        ~f:(fun x item ->
          if Char.equal item char
          then (
            char_x := x;
            char_y := y))
        list)
    array;
  !char_x, !char_y
;;

let get (coord : int * int) (array : char array array) : char =
  let x, y = coord in
  Array.get (Array.get array y) x
;;

let offsets ~x ~y = [ x, y + 1; x + 1, y; x, y + -1; x + -1, y ]

let valid_neighbors (coords : int * int) (array : char array array)
  : (int * int) list
  =
  let x, y = coords in
  let height = Array.length array in
  let width = Array.length (Array.get array 0) in
  let rec helper list =
    match list with
    | [] -> []
    | (x', y') :: tl ->
      if x' < width && x' >= 0 && y' >= 0 && y' < height
      then
        if not (Char.equal (get (x', y') array) '#')
        then (x', y') :: helper tl
        else helper tl
      else helper tl
  in
  helper (offsets ~x ~y)
;;

let rec dfs (array : char array array) (node : int * int) visited path =
  let visited = Set.add visited node in
  if Char.equal (get node array) 'E'
  then Some (node :: path)
  else
    List.fold (valid_neighbors node array) ~init:None ~f:(fun p_acc v ->
      if not (Set.mem visited v)
      then (
        match dfs array v visited (node :: path) with
        | Some p' -> Some p'
        | None -> p_acc)
      else p_acc)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let array = of_file input_file in
        let start = find 'S' array in
        let visited = Position.Set.of_list [] in
        let path = [] in
        let return =
          match dfs array start visited path with None -> [] | Some p -> p
        in
        List.iter (List.rev return) ~f:(fun (x, y) ->
          print_endline
            (("(" ^ Int.to_string x ^ ", " ^ Int.to_string y) ^ ")"))]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
