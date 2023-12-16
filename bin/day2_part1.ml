open Utils

let sum_valid_game_indices ggames =
  let rec process_games total index = function
    | [] -> total
    | game :: rest ->
      let game_moves1 = String.trim game |> String.split_on_char ':' in
      let game_moves = List.nth game_moves1 1 |> String.trim in
      let flag =
        List.for_all (fun turn ->
          List.for_all (fun moves ->
            let tokens = String.trim moves |> String.split_on_char ' ' |> Array.of_list in
            match String.trim tokens.(1) with
            | "blue" -> int_of_string (String.trim tokens.(0)) <= 14
            | "red" -> int_of_string  (String.trim tokens.(0))<= 12
            | "green" -> int_of_string (String.trim tokens.(0)) <= 13
            | _ -> false
          ) (String.split_on_char ',' turn)
        ) (String.split_on_char ';' game_moves)
      in
      if flag then process_games (total + index) (index + 1) rest
      else process_games total (index + 1) rest
  in
  process_games 0 1 ggames

let () =
  let filename = "day2_in.txt" in
  let games = read_lines_from_file filename in
  let result = sum_valid_game_indices games in
  prerr_int result;
