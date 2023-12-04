open Utils

  let patterns = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "zero"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"] 
  let pattern_regex = Str.regexp (String.concat "\\|" patterns) 


let find_last_match  text =
  let len = String.length text in
  try
    let _ = Str.search_backward pattern_regex text (len - 1) in
    let found = Str.matched_group 0 text in
    found
  with
  | Not_found -> "-1"




let find_first_match text =
  try
    let _ = Str.search_forward pattern_regex text 0 in
    let found = Str.matched_group 0 text in
    
    found
  with
  | Not_found -> "-1"


let concatenate_and_convert_to_int first_digit second_digit =
  let result_str =first_digit ^ second_digit in
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

  let first_match = find_first_match line in 
  let last_match = find_last_match line in  
   acc + concatenate_and_convert_to_int (tf first_match) (tf last_match)



let () =
  let filename = "day1_in.txt" in
  let lines = read_lines_from_file filename in
  let res = List.fold_left (fun acc line -> apply acc line) 0 lines in 
  print_int res;
