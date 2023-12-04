(* utils.ml *)


let read_line () =
  try Some (input_line stdin) with End_of_file -> None

let read_single_line () =
  match read_line () with
  | Some str -> str
  | None -> ""

let print_list to_string lst =
  List.iter (fun elem -> print_endline (to_string elem)) lst



let read_lines_from_file filename =
  try
    Printf.printf "Trying to open file: %s\n" filename;
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        Printf.printf "Closed file: %s\n" filename;
        List.rev acc
    in
    read_lines []
  with Sys_error msg ->
    Printf.printf "Error opening file: %s\n" msg;
    []


