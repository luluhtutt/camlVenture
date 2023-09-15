type phrase = string

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

exception Empty
exception Malformed

(** [view_match p] is the view command corresponding to the passed in phrase [p] *)
let view_match = function
  | [] -> View Status (*Default viewing*)
  | [ h ] when h = "team" -> View Team
  | [ h ] when h = "status" -> View Status
  | [ h ] when h = "moves" -> View Moves
  | [ h ] when h = "opponent" -> View Oppo
  | h :: t when h = "camon" -> View (Camon (String.concat " " t))
  | _ -> raise Malformed

let parse str =
  let str_lst = String.split_on_char ' ' (String.lowercase_ascii str) in
  let remove_empty = List.filter (fun x -> x <> "") str_lst in
  match remove_empty with
  | [] | [ "" ] -> raise Empty
  | [ h ] ->
      if h = "quit" then Quit
      else if h = "view" then view_match []
      else if h = "help" then Help
      else raise Malformed
  | h :: t ->
      let phrase = String.concat " " t in
      if h = "use" then Use phrase
      else if h = "view" then view_match t
      else if h = "switch" then Switch phrase
      else raise Malformed
