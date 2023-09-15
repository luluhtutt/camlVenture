(** This module represents the states of two trainers as they undergo a battle. *)

(** type [turn] represents which turn of combat the battle is currently on *)
type turn =
  | O
  | P

(** type [battle_status] represents whether the player trainer has won or lost
    the battle, or that the battle is still in process *)
type battle_status =
  | Win
  | Lose
  | Prog of turn

type b
(** type [b] represents the current state of the battle *)

type stat = {
  s : battle_status;
  text : string list;
}
(** type [stat] represents the output after a move that affected status of a
    battle*)

type r =
  | Text of string list
  | BattleR of stat
      (** type [r] gives terminal the necessary inputs to display the right
          information. There are two scenarios.

          - [Text] is when there is only text because status did not change
            since the last return of this type
          - [BattleR] if a action affected the status of the battle. It is a
            package that has the status of the battle and a text message.*)

val init_battle : Trainerstate.t -> Trainerstate.t -> bool -> b
(** [init_battle tr oppo iswild] is the battle state of player trainer [tr] and
    its opponent [oppo]. Battle starts on the player's turn

    flag [iswild] differentiates whether this battle is against a hacker or a
    wild camon. *)

val pl_rnd : string -> b -> r
(** [pl_rnd cmd b] modifies the battle state with the result of the player
    taking the action described in the terminal command [cmd]. Returns a package
    describing the action to be displayed in terminal.

    - Switching camon -> Prog O
    - Using a move -> Prog O or Win
    - Viewing -> Prog P *)

val op_rnd : b -> r
(** [op_rnd b] modifies the battle state with the result of the opponents turn.
    Returns a package describing the action to be displayed in terminal.

    If their camon is Alive, use a random attack

    If camon is Defeated, switch camon *)
