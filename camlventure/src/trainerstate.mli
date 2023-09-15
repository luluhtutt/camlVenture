(**This module initalizes and manages the state of the trainer. The states of
   the trainer's camon change as the game progresses.*)

type t
(** The abstract type of values representing trainers. *)

(** the type camon_status represents whether the camon is defeated or alive*)
type camon_status =
  | Alive of int
  | Defeated

exception UnknownCamon of string
(** Raised when an unknown camon identifier is encountered. It carries the
    identifier of the unknown camon. *)

exception UnknownMove of string
(** Raised when an unknown move identifier is encountered. It carries the
    identifier of the unknown move. *)

val from_json : Yojson.Basic.t -> Yojson.Basic.t -> t
(** [from_json tr_j cm_j] is the trainer that [tr_j] represents with camon
    represented by [cm_j]. *)

(** [trainer] represents the information requested by the terminal. *)
type trainer =
  | Clarkson
  | Doenges
  | Hackling
  | Player of string
  | Wild

val get_trainer : trainer -> t
(** [get_trainer tr] is the information about [tr] that will be requested by the
    terminal

    - [Clarkson] -> initalize a trainerstate of Clarkson
    - [Doenges] -> initalize a trainerstate of Deoneges
    - [Hackling] -> initalize a trainerstate of a Hackling, with a random name
      and camon
    - [Player name] -> initalize a trainerstate of the player, set with title
      [Hero \[name\]]
    - [Wild] -> initalize a trainerstate for a wild camon*)

val trainer_ascii : t -> string
(** [trainer_ascii tr] is the ascii file name of trainer [tr]. *)

val trainer_title : t -> string
(** [trainer_title tr] is the title of trainer [tr] *)

val quote : t -> bool -> string
(** [quote tr won] is the trainer [tr] win quote if [won] is true, else it is
    the lose quote*)

val active_camon : t -> string
(** [active_camon st] is the identifier of the camon in which the trainer has as
    its active camon in the current state [st]. *)

val camon_team : t -> string list
(** [camon_team] is a list of the names of the camon in trainer [tr] camon team,
    including the trainer's active camon*)

val camon_hp : t -> string -> int
(** [camon_hp tr name] is the current hp of the camon named [name] of the
    trainer [tr]. In the initial state, the camon has full hp level

    raise [UnknownCamon camon_name] if the camon [name] is does not exist in
    trainer [tr] team*)

val get_max_hp : t -> int
(** [get_max_hp ts] is the integer representing the maximum amount of hp a
    trainer's active camon can have. *)

val get_hp_perc : t -> float
(** [get_hp_perc ts] returns the percentage of hp left in the form of a float. *)

val get_camon_ascii : t -> string
(** [get_ascii ts] is the string name of the ascii file associated with the
    trainerstates active camon*)

val add_camon : t -> Yojson.Basic.t -> unit
(** [set_camon tr cm_j] adds the camon represented by [cm_j] added to the camon
    team of trainer [tr]. *)

val attack_ids : t -> string -> string list
(** [attack_ids tr cm] is a list of the names of all the available moves of a
    camon with the name [cm]. Raise [UnknownCamon name] if there is no camon
    with that name in team *)

val attack_stats : t -> string -> string -> int * int
(** [attack_stats tr camon_name attack_id] is a tuple representing the damage
    and accuracy (in order) of the passed in move [attack_id] from camon
    [camon_name] in trainer [tr].

    Raise [UnknownCamon camon_name] if there is no camon with name [camon_name]
    in team.

    Raise [UnknownMove attack_id] if there is no move with identifier
    [attack_id] in the camon's list of moves *)

(** the type attack_result represents the result of the attack.*)
type attack_result =
  | Success of camon_status
  | Miss of camon_status
  | Illegal

(** [switch_result] represents the result of the camon switch. [Legal t] is a
    successful switch where [t] is the new player's state with a new active
    camon. [Dead] is the result of a switch to a existing but defeated camon.
    [Illegal] is an attempt to switch to a non-existing camon of the trainer's
    team*)
type switch_result =
  | Legal
  | Dead
  | Illegal

val use : string -> t -> t -> attack_result
(** [use move pl op] is the result of attempting to use trainer [pl]'s active
    camon move [move] against the active camon of the opposing state [op]:

    - If [move] is the name of a move from the trainer's active camon, then the
      result is:
    - [Success st'] if the attack hits the opponent's camon, where in [st'] are
      the results of the attack on the opponent camon.
    - [Miss st'] if the attack misses the opponent's camon. No damage is dealt,
      [st'] is the unchanged oppoment camon status.

    - Otherwise, the result is [Illegal].

    [use] does not print anything. *)

val switch : string -> t -> switch_result
(** [switch camon_name tr st] is the result of attempting to switch active camon
    of trainer [tr] with camon named [camon_name] in state [st]:

    - If [camon_name] is the name of a camon from the trainer's current team,
      then the result is [Legal], and the trainer now has the camon named
      [camon_name] as its active camon. Else the result could also be [Dead]
      which indicates a switch to an existing but defeated camon.

    - Otherwise, the result is [Illegal].

    [switch] does not print anything. *)

val switch_alive : t -> bool
(** [switch_alive tr] switches the active camon of the trainerstate [tr] to an
    undefeated camon on their team if one exists.

    Returns [true] if the switch is successful, [false] otherwise *)
