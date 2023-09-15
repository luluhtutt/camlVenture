open Yojson.Basic.Util

(* initalize random generator *)
let _ = Random.self_init ()

exception UnknownCamon of string
exception UnknownMove of string

type camon_status =
  | Alive of int
  | Defeated

type attack_result =
  | Success of camon_status
  | Miss of camon_status
  | Illegal

type switch_result =
  | Legal
  | Dead
  | Illegal

type m = {
  attack_id : string;
  damage : int;
  accuracy : int;
}
(** abstract type m represents a camon's moves *)

type c = {
  name : string;
  max_hp : int;
  mutable hp : camon_status;
  moves : m list;
  ascii : string;
}
(** abstract type c represents a camon *)

type t = {
  mutable title : string;
  mutable active : c;
  mutable team : c list;
  win_quote : string;
  lose_quote : string;
  ascii : string;
}

(** [camon_from_json mj] is the move that mj represents *)
let moves_from_json mj =
  {
    attack_id = mj |> member "attack_id" |> to_string;
    damage = mj |> member "damage" |> to_int;
    accuracy = mj |> member "accuracy" |> to_int;
  }

(** [camon_from_json cmj] is the camon that [cm_j] represents*)
let camon_from_json cmj =
  {
    name = cmj |> member "name" |> to_string;
    max_hp = cmj |> member "hp" |> to_int;
    hp = Alive (cmj |> member "hp" |> to_int);
    moves = cmj |> member "moves" |> to_list |> List.map moves_from_json;
    ascii = cmj |> member "ascii" |> to_string;
  }

let from_json trj cmj =
  let camon = camon_from_json cmj in
  {
    title = trj |> member "title" |> to_string;
    active = camon;
    team = [ camon ];
    win_quote = trj |> member "win_quote" |> to_string;
    lose_quote = trj |> member "lose_quote" |> to_string;
    ascii = trj |> member "ascii" |> to_string;
  }

let trainer_ascii tr = tr.ascii
let trainer_title tr = tr.title
let quote tr have_won = if have_won then tr.win_quote else tr.lose_quote
let active_camon tr = tr.active.name

(** [camon_list lst] takes in a list of camon [lst] and concates all of their
    names into a list*)
let rec camon_list = function
  | [] -> []
  | h :: t -> h.name :: camon_list t

let camon_team tr = camon_list tr.team

(** [get_camon trainer name] is the camon with name [name] trainer [tr]'s camon
    team. Raise [UnknownCamon name] if there is no camon with that name in team *)
let get_camon tr name =
  let camon = List.find_opt (fun x -> x.name = name) tr.team in
  match camon with
  | Some c -> c
  | None -> raise (UnknownCamon name)

let camon_hp tr name =
  try
    match (get_camon tr name).hp with
    | Alive hp -> hp
    | Defeated -> 0
  with UnknownCamon a -> raise (UnknownCamon a)

let get_max_hp ts = ts.active.max_hp

let get_hp_perc ts =
  float_of_int (camon_hp ts ts.active.name) /. float_of_int ts.active.max_hp

let get_camon_ascii ts = ts.active.ascii
let add_camon tr cmj = tr.team <- camon_from_json cmj :: tr.team

(** [move_list lst] takes in a list of moves [lst] and concates all of their
    identifiers into a list*)
let rec move_list = function
  | [] -> []
  | h :: t -> h.attack_id :: move_list t

let attack_ids tr name =
  let cm = get_camon tr name in
  move_list cm.moves

(** [get_move trainer name attack_id] is move with identifier [attack_id] of the
    camon with name [name] trainer [tr]'s camon team.

    Raise [UnknownCamon name] if there is no camon with name [name] in team

    Raise [UnknownMove attack_id] if there is no move with identifier
    [attack_id] in the camon's list of moves*)
let get_move tr name attack_id =
  let camon = get_camon tr name in
  let move = List.find_opt (fun x -> x.attack_id = attack_id) camon.moves in
  match move with
  | Some m -> m
  | None -> raise (UnknownMove attack_id)

let attack_stats tr name attack_id =
  let move = get_move tr name attack_id in
  (move.damage, move.accuracy)

(** [hit_or_miss a] takes in the accuracy level of a move [a] and randomly
    determine whether the move was successful or not based on the probability
    given by the accuracy level*)
let hit_or_miss a = Random.full_int 100 + 1 <= a

(** [lower_hp st dmg] is the remaining hp of the active camon of the state [st]
    after hit with an attack that lowers its hp by the amount of [dmg] taken*)
let lower_hp tr dmg =
  match tr.active.hp with
  | Alive x ->
      let res = x - dmg in
      if res <= 0 then
        let _ = tr.active.hp <- Defeated in
        Success Defeated
      else
        let _ = tr.active.hp <- Alive res in
        Success tr.active.hp
  | Defeated -> failwith "Impossible: Cannot attack a defeated camon"

let use m pl op =
  try
    let dmg, att = attack_stats pl pl.active.name m in
    if hit_or_miss att then lower_hp op dmg else Miss op.active.hp
  with UnknownMove _ -> Illegal

let switch cm tr =
  try
    let c = get_camon tr cm in
    match c.hp with
    | Alive _ ->
        tr.active <- c;
        Legal
    | Defeated -> Dead
  with UnknownCamon _ -> Illegal

let switch_alive tr =
  let alive = List.filter (fun c -> c.hp <> Defeated) tr.team in
  match alive with
  | [] -> false
  | h :: t ->
      tr.active <- h;
      true

type trainer =
  | Clarkson
  | Doenges
  | Hackling
  | Player of string
  | Wild

(** [get_trainer_json s] returns the Yojson.Basic representation of the trainer
    [s].json *)
let get_trainer_json s =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "trainer" ^ Filename.dir_sep ^ s ^ ".json")

(** [get_camon_json s] returns the Yojson.Basic representation of the camon
    [s].json *)
let get_camon_json s =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "camon" ^ Filename.dir_sep ^ s ^ ".json")

(** [rand_name ()] returns the string name of a random element of the element of
    the string list [lst] *)
let rand_name lst = Random.int (List.length lst) |> List.nth lst

(** [change_name n t] adds the name [n] to the title of trainer [t] and returns
    the modified trainerstate *)
let change_name n t =
  t.title <- t.title ^ n;
  t

let get_trainer t =
  let names = Yojson.Basic.from_file "data//names.json" in
  let camons = names |> member "camons" |> to_list |> List.map to_string in
  let hacklings =
    names |> member "hacklings" |> to_list |> List.map to_string
  in
  match t with
  | Clarkson -> from_json (get_trainer_json "clarkson") (get_camon_json "ocaml")
  | Doenges -> from_json (get_trainer_json "doenges") (get_camon_json "heron")
  | Hackling ->
      from_json
        (get_trainer_json "hackling")
        (get_camon_json (rand_name camons))
      |> change_name (rand_name hacklings)
  | Player n ->
      let tr =
        from_json (get_trainer_json "base") (get_camon_json "jocalf")
        |> change_name n
      in
      add_camon tr (get_camon_json "ducky");
      tr
  | Wild ->
      from_json (get_trainer_json "null") (get_camon_json (rand_name camons))
