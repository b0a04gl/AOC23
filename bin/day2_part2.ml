open Utils

let sum_valid_game_indices ggames =
  let rec process_games countGame = function
    | [] -> countGame
    | game :: rest ->
      let game_moves1 = String.trim game |> String.split_on_char ':' in
      let game_moves = List.nth game_moves1 1 |> String.trim in
      let minb, ming, minr = ref min_int, ref min_int, ref min_int in
      List.iter (fun turn ->
        List.iter (fun moves ->
          let tokens = String.trim moves |> String.split_on_char ' ' |> Array.of_list in
          let cnt = int_of_string tokens.(0) in
          match String.trim tokens.(1) with
          | "blue" -> minb := max !minb cnt
          | "red" -> minr := max !minr cnt
          | "green" -> ming := max !ming cnt
          | _ -> ()
        ) (String.split_on_char ';' turn)
      ) (String.split_on_char ',' game_moves);
      process_games (countGame + (!minb * !ming * !minr)) rest
  in
  process_games 0 ggames

let () =
  let filename = "day2_in.txt" in
  let games = read_lines_from_file filename in
  let result = sum_valid_game_indices games in
  prerr_int result;
