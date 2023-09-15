open Notty
open Notty_unix
open Ascii
open Battle
open Trainerstate

exception InvalidCell

let _ = Random.self_init ()

(** The abstract type representing a cell on the map. *)
type cell =
  | Player
  | Empty
  | PalmTree
  | Cactus
  | Hackling
  | Doenges
  | Clarkson
  | Wild

(** The abstract type representing the current state of the UI*)
type state =
  | Map
  | Battle
  | Complete

(** The abstract type representing the current state of the battle. *)
type battle =
  | PreBattle
  | Encounter
  | Fight
  | Won_Battle
  | Victory
  | Lost_Battle
  | Defeat

(** [states] is a state ref that represents the current state of the game -
    either Map or Battle.*)
let states = ref Map

(** [width] represents the width by character spaces. *)
let width = 60

(* [height] represents the height by character spaces. *)
let height = 50

(* [player_position] represents the player's initial starting coordinates. *)
let player_position = ref (5, 25)

(** [clarkson_position] represents Snarly Hacker Clarkson's coordinates. *)
let clarkson_position = (45, 25)

(** [doenges_position] represents Snarly Apprentice Doenges's coordinates. *)
let doenges_position = (20, 25)

(** [world] is the cell 2D array/matrix that represents the map/world. *)
let world = Array.make_matrix width height Empty

(** [default_help] is a string list that contains the help instructions. *)
let default_help =
  [
    "Try to get your opponents hp to zero using one of your moves! type \"Use \
     <move_name>\" to use the move specified.";
    "To switch your active camon with one on your team type \"Switch \
     <camon_name>\"";
    "To see more information type \"View\" followed by whatever you are \
     interested in (team, status, moves, camon)";
  ]

(** [info_ref] is a string list ref that represents the information text during
    battle. *)
let info_ref = ref default_help

(** [player_trainerstate] is a Trainerstate.t representing the player's
    Trainerstate. *)
let player_trainerstate = Trainerstate.get_trainer (Trainerstate.Player "name")

(** [opponent_trainerstate] is a Trainerstate.t ref representing the current
    opponent's Trainerstate. *)
let opponent_trainerstate = ref (Trainerstate.get_trainer Trainerstate.Hackling)

(** [current_battle] is a Battle.b ref that represents the current battle.*)
let current_battle =
  ref (Battle.init_battle player_trainerstate !opponent_trainerstate false)

(** [battle_state] is a battle ref that represents the current stage of the
    current Battle - PreBattle, Encounter, Fight, Won_Battle, Lost_Battle,
    Victory, or Defeat. *)
let battle_state = ref PreBattle

(** [player_turn] is true when it is currently the user's turn and false if not. *)
let player_turn = ref true

(** [current_move] is a string reference to the player's current move. *)
let current_move = ref ""

(** [current_enemy] is a cell ref representing the current enemy on the Map. *)
let current_enemy = ref Player

(** [get_cell (x, y)] returns the cell type of the cell at position (x,y) in the
    world. Raises [InvalidCell] if (x,y) is out of range. *)
let get_cell (x, y) =
  try world.(x).(y) with
  | Invalid_argument _ -> raise InvalidCell
  | _ -> world.(x).(y)

(** [set_cell (x, y) c] sets the cell at position (x, y) to a cell of ype c. *)
let set_cell (x, y) c = world.(x).(y) <- c

(** [is_enemy (x,y)] returns true if the cell at position (x, y) is an enemy,
    and false otherwise. *)
let is_enemy (x, y) =
  match get_cell (x, y) with
  | Doenges ->
      current_enemy := Doenges;
      true
  | Clarkson ->
      current_enemy := Clarkson;
      true
  | Hackling ->
      current_enemy := Hackling;
      true
  | Wild ->
      current_enemy := Wild;
      true
  | _ -> false

(** [generate_map] adds different cells to the world array. 50 cacti are placed
    randomly, and the player and enemy trainer are placed in their designated*)
let generate_map =
  for i = 0 to 50 do
    set_cell (Random.int width, Random.int height) PalmTree
  done;
  for i = 0 to 50 do
    set_cell (Random.int width, Random.int height) Cactus
  done;
  for i = 0 to 15 do
    set_cell (Random.int width, Random.int height) Hackling
  done;
  for i = 0 to 25 do
    set_cell (Random.int width, Random.int height) Wild
  done;
  set_cell clarkson_position Clarkson;
  set_cell doenges_position Doenges;
  set_cell !player_position Player

(** [image_of_cell c] returns the Notty.image representation of the cell c. *)
let image_of_cell c =
  match c with
  | PalmTree -> I.string A.empty "\u{1F334}"
  | Cactus -> I.string A.empty "\u{1F335}"
  | Player -> I.string A.empty "\u{1F47E}"
  | Hackling -> I.string A.empty "\u{1F916}"
  | Doenges -> I.string A.empty "\u{1F621}"
  | Clarkson -> I.string A.empty "\u{1F608}"
  | Wild -> I.string A.empty "\u{1F95A}"
  | _ -> I.string A.empty " "

(** [navigation_keys t] matches a terminal event t with certain keys pressed.
    Arrow keys return a tuple (dx, dy) and esc quits the program.*)
let navigation_keys t =
  match Term.event t with
  | `Key (`Arrow `Up, _) -> (0, -1)
  | `Key (`Arrow `Down, _) -> (0, 1)
  | `Key (`Arrow `Right, _) -> (1, 0)
  | `Key (`Arrow `Left, _) -> (-1, 0)
  | `Key (`Escape, _) -> exit 0
  | _ -> (0, 0)

(** (x1, y1) + (x2, y2) returns the new coordinates (x1+x2, y1+y2). Referenced
    from lab 6. Used to add coordinates. *)
let ( + ) (dx, dy) (xi, yi) = (xi + dx, yi + dy)

(** [update_pos (xi, yi) t] returns a new position (xi+dx, yi+dy) based on
    player movement using keyboard arrows. If the player steps on a cell with an
    enemy, a battle is initiated. *)
let update_pos cur_pos t =
  let new_pos = cur_pos + navigation_keys t in
  if is_enemy new_pos then (
    battle_state := Encounter;
    states := Battle);
  set_cell cur_pos Empty;
  set_cell new_pos Player;
  new_pos

(** [match_r r] is a helper function that matches the player's command to battle
    functionality. *)
let match_r r =
  match r with
  | Text s -> info_ref := s
  | BattleR a -> (
      if !player_turn then player_turn := false else player_turn := true;
      info_ref := a.text;
      match a.s with
      | Win -> battle_state := Won_Battle
      | Lose -> battle_state := Lost_Battle
      | _ -> ())

(** [battle_keys t] matches a terminal event t with certain keys pressed during
    battle. These include moving forwards in the game by pressing tab or enter
    and typing commands.*)
let battle_keys t =
  match Term.event t with
  | `Key (`Tab, _) -> battle_state := Fight
  | `Key (`Escape, _) -> exit 0
  | `Key (`Backspace, _) ->
      if String.length !current_move > 0 then
        current_move :=
          String.sub !current_move 0 (String.length !current_move - 1)
      else if String.length !current_move = 0 then current_move := !current_move
  | `Key (`ASCII ac, _) -> current_move := !current_move ^ String.make 1 ac
  | `Key (`Enter, _) ->
      (match !battle_state with
      | Won_Battle -> battle_state := Victory
      | Lost_Battle -> battle_state := Defeat
      | Victory -> (
          match !current_enemy with
          | Clarkson -> states := Complete
          | _ ->
              states := Map;
              battle_state := PreBattle;
              player_turn := true;
              info_ref := default_help)
      | Defeat -> battle_state := Defeat
      | _ ->
          if !player_turn then
            match_r (Battle.pl_rnd !current_move !current_battle)
          else match_r (Battle.op_rnd !current_battle));
      current_move := ""
  | _ -> ()

(** [encounter t] returns an image of the initial encounter screen corresponding
    to the enemy trainer [t]. *)
let encounter trainer =
  opponent_trainerstate := trainer;
  let encounter_message =
    match !current_enemy with
    | Wild ->
        current_battle :=
          Battle.init_battle player_trainerstate !opponent_trainerstate true;
        "Wild Camon "
        ^ Trainerstate.active_camon trainer
        ^ " is charging at you!"
    | _ ->
        current_battle :=
          Battle.init_battle player_trainerstate !opponent_trainerstate false;
        Trainerstate.trainer_title trainer ^ " is challenging you to a battle! "
  in
  Ascii.read_file (Trainerstate.trainer_ascii trainer)
  @ Ascii.textbox encounter_message "Press tab to defend your honor!"
  |> List.map (fun i -> I.string A.empty i)
  |> I.vcat

(** [battle_textbox_helper b] returns a string list representing the textbox in
    battle. If it is the player's turn, the textbox is the move selection,
    otherwise it is a texbox telling the player to press enter.*)
let battle_textbox_helper b =
  if b then
    Ascii.move_select
      (Trainerstate.attack_ids player_trainerstate
         (Trainerstate.active_camon player_trainerstate))
  else Ascii.textbox "Press enter to proceed." ""

(** [fight t] returns the current battle screen corresponding to the enemy
    trainer [t]. *)
let fight trainer =
  let tb =
    match !battle_state with
    | Won_Battle ->
        Ascii.textbox
          (Trainerstate.trainer_title trainer
          ^ ": "
          ^ Trainerstate.quote trainer false)
          "Press enter to proceed."
    | Lost_Battle ->
        Ascii.textbox
          (Trainerstate.trainer_title trainer
          ^ ": "
          ^ Trainerstate.quote trainer true)
          "Press enter to proceed."
    | _ -> battle_textbox_helper !player_turn
  in
  Ascii.battle
    (Ascii.read_file (Trainerstate.get_camon_ascii trainer))
    (Trainerstate.active_camon trainer)
    (Trainerstate.get_hp_perc trainer)
    (Ascii.read_file (Trainerstate.get_camon_ascii player_trainerstate))
    (Trainerstate.active_camon player_trainerstate)
    (Trainerstate.get_hp_perc player_trainerstate)
    tb !info_ref !current_move

let draw t name =
  match !states with
  | Complete ->
      battle_keys t;
      let im =
        let l = Ascii.victory |> List.map (fun i -> I.string A.empty i) in
        List.append l
          [
            I.string
              A.(fg red)
              "Defend your nation in Camlventure II : The Rise of Doenges !";
          ]
        |> I.vcat
      in
      Term.image t im
  | Map ->
      let im =
        let instruct =
          Ascii.map_instructions
          |> List.map (fun i -> I.string A.empty i)
          |> I.vcat
        in
        List.append
          [
            world
            |> Array.map (fun col ->
                   col |> Array.map image_of_cell |> Array.to_list |> I.vcat)
            |> Array.to_list |> I.hcat;
          ]
          [ instruct ]
      in
      Term.image t (im |> I.hcat);
      player_position := update_pos !player_position t
  | Battle ->
      battle_keys t;
      let im =
        match !battle_state with
        | PreBattle ->
            Ascii.textbox "Ooopss you've entered the void."
              "Here you must battle despite not having encountered anyone."
            |> List.map (fun i -> I.string A.empty i)
            |> I.vcat
        | Encounter -> (
            battle_state := Fight;
            match !current_enemy with
            | Clarkson ->
                encounter (Trainerstate.get_trainer Trainerstate.Clarkson)
            | Doenges ->
                encounter (Trainerstate.get_trainer Trainerstate.Doenges)
            | Hackling ->
                encounter (Trainerstate.get_trainer Trainerstate.Hackling)
            | _ -> encounter (Trainerstate.get_trainer Trainerstate.Wild))
        | Fight | Won_Battle | Lost_Battle ->
            fight !opponent_trainerstate
            |> List.map (fun i -> I.string A.empty i)
            |> I.vcat
        | Victory ->
            Ascii.result_screen true name
              (Trainerstate.quote player_trainerstate true)
              (Trainerstate.get_camon_ascii player_trainerstate)
            |> List.map (fun i -> I.string A.empty i)
            |> I.vcat
        | Defeat ->
            Ascii.result_screen false name
              (Trainerstate.quote player_trainerstate false)
              (Trainerstate.get_camon_ascii player_trainerstate)
            |> List.map (fun i -> I.string A.empty i)
            |> I.vcat
      in
      Term.image t im

(* I.hcat and I.vcat concatenates lists of images horizontally and vertically,
   respectively, into a single image. I referenced
   https://hackmd.io/@yF_ntUhmRvKUt15g7m1uGw/BJBZ7TMeq for the map pattern
   matching. *)
