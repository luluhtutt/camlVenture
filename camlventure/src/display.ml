open Trainerstate

let switching res c =
  match res with
  | Legal -> [ "Congrats! The new active camon is now changed to " ^ c ]
  | Illegal ->
      [
        "Poopy!!! The camon does not exist in the current trainer's team. The \
         active camon is still the same";
        "Type 'View camons' to see the available camons on your team.";
      ]
  | Dead ->
      [
        "Poopy!!! " ^ c
        ^ " was found dead. Please switch to a camon that is alive.";
      ]

let attack res c m =
  let out = c ^ " attempts to use " ^ m ^ "! " in
  match res with
  | Success t -> (
      match t with
      | Alive k ->
          [
            out;
            "The attack HIT, the hp of the opponent's active camon is now \
             reduced to " ^ string_of_int k;
          ]
      | Defeated ->
          [
            out;
            "Congrats, the attack HIT and DEFEATED the opponent's active camon";
          ])
  | Miss t -> (
      match t with
      | Alive k ->
          [
            out;
            "Poopy!!! The attack MISSED, the hp of the opponent's active camon \
             remained at " ^ string_of_int k;
          ]
      | _ -> failwith "The opponent has dead active camon active")
  | Illegal ->
      [
        out;
        "Poopy!!! The move does not exist for the current active camon. Please \
         try again. Type 'View moves' to see available moves";
      ]

let oppo_attack res c m =
  let out = "opponent attempts to use " ^ m ^ " of " ^ c ^ " to attack you. " in
  match res with
  | Success t -> (
      match t with
      | Alive k -> [ out; "Aaaa the opponent hit you. eMOtiONal dAMaGe" ]
      | Defeated -> [ out; "O Nooooo your camon has migrated to heaven" ])
  | Miss t -> [ out; "Woho keep strong. You did not get hit." ]
  | Illegal ->
      [
        out;
        "Impossible: opponent made an illegal move.";
        "There has been a rift in the space rift in the space time continuum. \
         Run while you still can";
      ]

(**[camon_hp tr name] gives the hp of the camon in string. For example, if the
   hp is 1 then the function gives (hp: 1)*)
let string_of_hp tr name = " (hp: " ^ string_of_int (camon_hp tr name) ^ ")"

(**[string_of_stats tr name attack] gives a string indicating damage and
   accuracy. For example, if the damage is 1 and accuracy is 3 then the function
   gives (damage: 1, accuracy: 3)*)
let string_of_stats tr name attack =
  match attack_stats tr name attack with
  | dmg, acc ->
      " (damage: " ^ string_of_int dmg ^ ", accuracy: " ^ string_of_int acc
      ^ ")"

(** [iter lst idx] gives a string that appends each element in [lst] *)
(* let rec iter m = match m with | [] -> "" | h :: t -> h ^ " " ^ iter t *)

let moves tr c =
  List.map (fun x -> x ^ string_of_stats tr c x) (attack_ids tr c)

let camons tr = List.map (fun x -> x ^ string_of_hp tr x) (camon_team tr)

let status tr c =
  try
    [
      "Current camon name: " ^ c ^ "; hp: " ^ string_of_int (camon_hp tr c);
      "Available moves: ";
    ]
    @ moves tr c
  with UnknownCamon c ->
    [ "Poopy! You made a boo boo! " ^ c ^ " does not exist in your team" ]
