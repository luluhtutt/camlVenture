exception InvalidCell
(** Raised when trying to access a cell that is out of bounds or is occupied*)

val draw : Notty_unix.Term.t -> string -> unit
(** [draw t n] generates a Notty image to display as a TUI, depending on the
    current state. [t] represents a terminal instance, and [n] is the player's
    name. *)