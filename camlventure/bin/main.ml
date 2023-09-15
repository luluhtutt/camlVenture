open Notty
open Notty_unix
open Camlventure
open Ui

(** [print_coordinate (x, y)] returns a string describing the coordinate and
    whether or not the cell at (x, y) is an enemy. *)
(* let print_coordinate (x, y) = String.cat (String.cat (string_of_int x)
   (String.cat ", " (string_of_int y))) (string_of_bool (is_enemy (x, y))) *)

(** [play_game f] starts the adventure in file [f]. *)
let play_game n =
  (* let player = Trainerstate.get_trainer (Player n) in *)
  let terminal = Term.create () in
  while true do
    draw terminal n (* print_endline (print_coordinate !player_pos) *)
  done

(** [get_lore ()] returns a string list describing the introductory lore of this
    world*)
let get_lore () =
  let open Yojson.Basic.Util in
  let lore = Yojson.Basic.from_file "data//lore.json" in
  lore |> member "intro" |> to_list |> List.map to_string

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Camlventure\n";
  List.iter (fun x -> print_endline x) (get_lore ());
  print_endline "\nPlease enter your name to begin your journey";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name -> play_game name

(** [let ()] generates and runs the TUI in the terminal instance terminal. *)
let () = main ()
