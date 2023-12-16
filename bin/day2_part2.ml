open Utils

let () =
  let filename = "day2_in.txt" in
  let lines = read_lines_from_file filename in
  print_int (List.length lines);


