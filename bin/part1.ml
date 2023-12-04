open Utils


let extract_and_sum acc input =
  let nums = Str.global_replace (Str.regexp "[a-z]") "" input in
  let first_num = Str.first_chars nums 1 in
  let last_num = String.sub nums (String.length nums - 1) 1 in
  let result = (int_of_string first_num)*10 + int_of_string last_num in
  print_endline input;
  print_endline (first_num^","^ last_num);
  acc + result


let () =
  let filename = "day1_in.txt" in
  let lines = read_lines_from_file filename in
  let res = List.fold_left (fun acc line -> extract_and_sum acc line) 0 lines in 
  print_int res;


