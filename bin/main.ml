open Game

let play_helper players =
  match players with
  | i -> print_endline "Make turn"

let play_game number_player =
  match number_player with
  | i when i < 5 ->
      play_helper i;
      exit 0
  | i when i < 8 ->
      play_helper i;
      exit 0
  | _ -> exit 1

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\n Welcome to Go Fish.\n";
  print_endline "Please enter the number of players for the game.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number -> play_game (int_of_string number)

(* Execute the game engine. *)
let () = main ()
