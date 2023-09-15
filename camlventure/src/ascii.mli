val read_file : string -> string list
(** [read_file fn] reads a file in the ascii folder into a string list.*)

val textbox : string -> string -> string list
(** [textbox s1 s2] produces a textbook with content s1 and s2 on the middle two
    lines, respectively. This textbox is represented in the form of a string
    list. *)

val battle_box : int -> string -> float -> string list
(** [battle_box p n h] draws a camon's information box during battle. [p]
    indicates which side the camon is on - 0 for the player, 1 for the opponent.
    [n] is the camon's name, and [h] is a float between 0 and 1 representing
    what fraction of the camon's health remains. *)

val battle_camon : string list -> int -> string list
(** [battle_camon clist p] is a string list representing a camon's ascii art
    sprite in battle. [clist] is the camon's raw ascii art, and [p] indicates
    which side the camon is on - 0 for the player, 1 for the opponent. *)

val battle :
  string list ->
  string ->
  float ->
  string list ->
  string ->
  float ->
  string list ->
  string list ->
  string ->
  string list
(** [battle e_c_file e_c_name e_health p_c_file p_c_name p_health tb info input]
    represents a battle screen. The inputs include the enemy's and the player's
    camon art, name, and health as well as textbox information and any user
    input. *)

val move_select : string list -> string list
(** [moves sl] represents the player's camon's moves, from which the player will
    choose which one to play. *)

val result_screen : bool -> string -> string -> string -> string list
(** [results_screen pl_w name quote camon_fn] represents the screen shown after
    a battle is won or lost. *)

val victory : string list
(** [victory] is the screen shown after your defeat Snarly Hacker Clarkson. *)

val map_instructions : string list
(** [map_instructions] are instructions for navigating the map. *)