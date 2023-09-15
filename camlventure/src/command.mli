(** This module parses the players' commands *)

exception Empty
(**Raises an exception if command is empty*)

exception Malformed
(**Raises an exception if command is malformed*)

type phrase = string
(** [phrase] is the words following the key command key words. Phrases contain
    no extra white space and are all lowercase. Examples of such phrase includes
    name of the move that the player wishes to make and the camon that the
    player want to switch to*)

(** [command] is the type of command that the player has inputted. [Use p]
    represents a [command] to make the move [p]. [View] indicates that the
    player wishes to obtain information see the [p] of their state. [Switch] is
    a command to switch from the current active camon into the one indicated by
    [p]. [Quit] is the type of [command] to exit the game *)
type view =
  | Team
  | Status
  | Moves
  | Oppo
  | Camon of phrase

type command =
  | Use of phrase
  | View of view
  | Switch of phrase
  | Help
  | Quit

val parse : string -> command
(** [parse str] parses the command [str] of the player into a [command].

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "do" nor "view" nor "switch" nor "quit", or if the verb
    is "quit" and there is a non-empty object phrase, or if the verb is "go" and
    there is an empty object phrase.

    Ex: ["      Switch three   humped camel"] would become the command
    [Switch "three humped camel"]*)
