open Utils

let iterate_substrings_right_to_left s =
  let len = String.length s in
  
  let patterns = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "zero"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"] in

  let pattern_regex = Str.regexp (String.concat "\\|" patterns) in

  let rec iterate_substrings_recursive start_len =
    if start_len > len then
      None
    else
      let substr = String.sub s (len - start_len) start_len in
      try
        let _ = Str.search_forward pattern_regex substr 0 in
        let found = Str.matched_group 0 substr in
        Some found
      with
      | Not_found -> iterate_substrings_recursive (start_len + 1)
  in

  match iterate_substrings_recursive 1 with
  | Some result -> result
  | None -> "$"




let iterate_substrings_left_to_right s =
  let len = String.length s in
  
  let patterns = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "zero"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"] in
  let pattern_regex = Str.regexp (String.concat "\\|" patterns) in

  let rec iterate_substrings_recursive start_len acc =
    if start_len > len then
      None
    else
      let substr = String.sub s 0 start_len in
      try
        let _ = Str.search_forward pattern_regex substr 0 in
       Some (Str.matched_group 0 s)
      with
      | Not_found -> iterate_substrings_recursive (start_len + 1) acc
  in

  match iterate_substrings_recursive 1 None with
  | Some result -> result
  | None -> "$"



let concatenate_and_convert_to_int first_char second_char =
  let result_str =first_char ^ second_char in
  print_endline result_str;
  int_of_string result_str

let tf term = match term with
| "one" -> "1"
| "two" -> "2"
| "three" -> "3"
| "four" -> "4"
| "five" -> "5"
| "six" -> "6"
| "seven" -> "7"
| "eight" -> "8"
| "nine" -> "9"
| "zero" -> "0"
| _ -> term



  

let apply acc line =
  print_endline ("word" ^ line); 
  let f = iterate_substrings_left_to_right line in 
  let l = iterate_substrings_right_to_left line in
  print_endline f;
  print_endline l;
  print_endline ("sum till now : " ^ string_of_int (    acc + concatenate_and_convert_to_int (tf f) (tf l) ) );
  acc + concatenate_and_convert_to_int (tf f) (tf l)

let () =
  let filename = "day1_in.txt" in
  let lines = read_lines_from_file filename in
  let res = List.fold_left (fun acc line -> apply acc line) 0 lines in 
  print_int res;


