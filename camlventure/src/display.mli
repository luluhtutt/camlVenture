(** This module converts the result of attacks and the necessary informations
    about camon and trainer into string so that it may be displayed to the users *)

val switching : Trainerstate.switch_result -> string -> string list
(** [switching res s] converts the result [res] of switching camon to what will
    be displayed to the player. [s] is the name of the current active camon *)

val attack : Trainerstate.attack_result -> string -> string -> string list
(** [attack res c m] converts the result [res] of the move [m] done by camon [c]
    to what will be displayed to the player.*)

val oppo_attack : Trainerstate.attack_result -> string -> string -> string list
(** [oppo_attack res c m] is the string to be displayed after the computer uses
    camon [c] to make move [m] on the player's camon*)

val moves : Trainerstate.t -> string -> string list
(** [moves tr c] provides the moves available for trainer [tr] with camon [c]
    listed as 1. 2. ...*)

val camons : Trainerstate.t -> string list
(** [canons st] provides the camons currently on the trainer's team at state
    [st] and list with 1. 2. ...*)

val status : Trainerstate.t -> string -> string list
(** [status tr c] converts the current status of the trainer [tr] with camon [c]
    to what will be displayed to the player*)
