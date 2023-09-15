(* initalize random generator *)
let _ = Random.self_init ()

type turn =
  | O
  | P

type battle_status =
  | Win
  | Lose
  | Prog of turn

type b = {
  pl : Trainerstate.t;
  op : Trainerstate.t;
  mutable status : battle_status;
  iswild : bool;
}

type stat = {
  s : battle_status;
  text : string list; (* camon : string; hp : int; *)
}

type r =
  | Text of string list
  | BattleR of stat

let init_battle pl op iswild = { pl; op; status = Prog P; iswild }

(** [update_status b camon_status] checks the health of the active camon to
    determine if there has been a victor and updates the current battle status.

    - If player is defeated -> Lose
    - If opponent is defeated -> Win
    - Else -> alternate turn *)
let update_status b cs : unit =
  match b.status with
  | Win -> failwith "Impossible: Battle is already won"
  | Lose -> failwith "Impossible: Battle is already Lost"
  | Prog t -> begin
      match t with
      | O -> begin
          match cs with
          | Trainerstate.Alive _ -> b.status <- Prog P
          | Trainerstate.Defeated ->
              if Trainerstate.switch_alive b.pl then b.status <- Prog P
              else b.status <- Lose
        end
      | P -> begin
          match cs with
          | Trainerstate.Alive _ -> b.status <- Prog O
          | Trainerstate.Defeated ->
              if Trainerstate.switch_alive b.op then b.status <- Prog O
              else b.status <- Win
        end
    end

(** [use_helper m b is_pl_turn] is the effect of attempting to use the move [m]
    on the opposing camon on the battle [b]. The attacker and the target is
    determined by [is_pl_turn]*)
let use_helper m b is_pl_turn =
  let open Display in
  let attacker = if is_pl_turn then b.pl else b.op in
  let victim = if is_pl_turn then b.op else b.pl in
  let res = Trainerstate.use m attacker victim in
  (* let c_victim = Trainerstate.active_camon victim in *)
  let t =
    (if is_pl_turn then attack else oppo_attack)
      res
      (Trainerstate.active_camon attacker)
      m
  in
  match res with
  | Success s | Miss s ->
      update_status b s;
      BattleR
        {
          s = b.status;
          (*Game should end if terminal see win/lose as status*)
          text =
            t
            (* camon = c_victim; hp = Trainerstate.camon_hp victim c_victim; *);
        }
  | _ -> Text t

(** [view_helper b cmd] is the effect of calling view based on the parsed
    message [cmd]*)
let view_helper b cmd =
  let ac = Trainerstate.active_camon b.pl in
  match cmd with
  | Command.Team -> Text (Display.camons b.pl)
  | Command.Status -> Text (Display.status b.pl ac)
  | Command.Moves -> Text (Display.moves b.pl ac)
  | Command.Camon c -> Text (Display.status b.pl c)
  | Command.Oppo -> Text (Display.status b.op (Trainerstate.active_camon b.op))

(** [switch_helper b c] is the effect of attempting to switch the current active
    camon with camon [c] on the current battle [b] *)
let switch_helper b c =
  let open Trainerstate in
  let res = switch c b.pl in
  Text (Display.switching res c)

let pl_rnd input b =
  try
    if b.status = Prog P then
      match Command.parse input with
      (* match the parsing with the appropriate function corresponding to what
         the trainer does with their turn *)
      | Use m -> use_helper m b true
      | View cmd -> view_helper b cmd
      | Switch c -> switch_helper b c
      | Help ->
          Text
            [
              "Try to get your opponents hp to zero using one of your moves! \
               type \"Use <move_name>\" to use the move specified.";
              "To switch your active camon with one on your team type \"Switch \
               <camon_name>\"";
              "To see more information type \"View\" followed by whatever you \
               are interested in (team, status, moves, camon)";
            ]
      | Quit ->
          if b.iswild then (
            b.status <- Win;
            BattleR
              {
                s = b.status;
                (*Game should end if terminal see win/lose as status*)
                text = [ "Got away safely!" ];
              })
          else Text [ "You can't run from a trainer battle!" ]
    else failwith "not player turn!"
  with Command.Empty | Command.Malformed ->
    Text [ "What was that?"; "Type \"Help\" to see all available commands" ]

let op_rnd b =
  (* choose a random move from the active camon *)
  let camon = Trainerstate.active_camon b.op in
  let moves = Trainerstate.attack_ids b.op camon in
  Random.int (List.length moves) |> List.nth moves |> fun rand ->
  use_helper rand b false
