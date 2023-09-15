open Battle

let read_file fn =
  let filename = "data" ^ Filename.dir_sep ^ "ascii" ^ Filename.dir_sep ^ fn in
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true do
      lines := input_line channel :: !lines
    done;
    !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines

let textbox s1 s2 =
  let lines = ref [] in
  lines := String.make 100 '=' :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  lines :=
    ("|" ^ String.make 10 ' ' ^ s1
    ^ String.make (88 - String.length s1) ' '
    ^ "|")
    :: !lines;
  lines :=
    ("|" ^ String.make 10 ' ' ^ s2
    ^ String.make (88 - String.length s2) ' '
    ^ "|")
    :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  lines := String.make 100 '=' :: !lines;
  List.rev !lines

let battle_box p c h =
  let lines = ref [] in
  if p = 1 then lines := String.make 50 ' ' :: !lines
  else
    for i = 0 to 10 do
      lines := String.make 50 ' ' :: !lines
    done;
  lines := (" " ^ String.make 48 '=' ^ " ") :: !lines;
  lines := (" " ^ "|" ^ String.make 46 ' ' ^ "|" ^ " ") :: !lines;
  lines :=
    (" " ^ "|     " ^ c ^ String.make (41 - String.length c) ' ' ^ "|" ^ " ")
    :: !lines;
  lines := (" " ^ "|" ^ String.make 46 ' ' ^ "|" ^ " ") :: !lines;
  lines :=
    (" " ^ "|    " ^ "["
    ^ String.make (int_of_float (36. *. h)) '+'
    ^ String.make (36 - int_of_float (36. *. h)) ' '
    ^ "]" ^ "    |" ^ " ")
    :: !lines;
  lines := (" " ^ "|" ^ String.make 46 ' ' ^ "|" ^ " ") :: !lines;
  lines := (" " ^ String.make 48 '=' ^ " ") :: !lines;
  if p = 0 then lines := String.make 50 ' ' :: !lines
  else
    for i = 0 to 10 do
      lines := String.make 50 ' ' :: !lines
    done;
  List.rev !lines

let battle_camon clist p =
  let clength = List.length clist in
  let extra_lines = ref [] in
  for i = 0 to 17 - clength do
    extra_lines := String.make 50 ' ' :: !extra_lines
  done;
  if p = 0 then
    List.append !extra_lines (List.append clist [ String.make 50 ' ' ])
  else List.append (String.make 50 ' ' :: clist) !extra_lines

let battle e_camon e_c_name e_health p_camon p_c_name p_health tb info
    user_input =
  let e_battle_box = battle_box 1 e_c_name e_health in
  let e_battle_camon = battle_camon e_camon 1 in
  let p_battle_box = battle_box 0 p_c_name p_health in
  let p_battle_camon = battle_camon p_camon 0 in
  let lines = ref [] in
  for i = 0 to 18 do
    lines := (List.nth e_battle_box i ^ List.nth e_battle_camon i) :: !lines
  done;
  for i = 0 to 18 do
    lines := (List.nth p_battle_camon i ^ List.nth p_battle_box i) :: !lines
  done;
  List.append (List.rev !lines)
    (List.append tb (List.append info [ "> " ^ user_input ]))

let rec pad_move_list l =
  match List.length l with
  | 4 -> l
  | i -> pad_move_list (List.append l [ "" ])

let move_select moves =
  let move_list = pad_move_list moves in
  let lines = ref [] in
  lines := String.make 100 '=' :: !lines;
  lines :=
    ("|     What move would you like to use?" ^ String.make 61 ' ' ^ "|")
    :: !lines;
  lines := ("|" ^ String.make 98 '-' ^ "|") :: !lines;

  let moves_ascii = ref "" in
  for i = 0 to 2 do
    moves_ascii :=
      !moves_ascii ^ "|    " ^ List.nth move_list i
      ^ String.make (20 - String.length (List.nth move_list i)) ' '
  done;
  moves_ascii :=
    !moves_ascii ^ "|    " ^ List.nth move_list 3
    ^ String.make (19 - String.length (List.nth move_list 3)) ' '
    ^ "|";
  lines := !moves_ascii :: !lines;

  let spaces_ascii = ref "" in
  for i = 0 to 2 do
    spaces_ascii := !spaces_ascii ^ "|" ^ String.make 24 ' '
  done;
  spaces_ascii := !spaces_ascii ^ "|" ^ String.make 23 ' ' ^ "|";
  lines := !spaces_ascii :: !lines;
  lines := String.make 100 '=' :: !lines;
  List.rev !lines

let result_screen pl_w name quote camon_fn =
  let lines = ref [] in
  lines := (" " ^ String.make 98 '_' ^ " ") :: !lines;
  for i = 0 to 3 do
    lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines
  done;
  lines := ("|" ^ String.make 98 '=' ^ "|") :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  if pl_w then
    lines := ("|       Congratulations" ^ String.make 76 ' ' ^ "|") :: !lines
  else lines := ("|       Boohoo :(" ^ String.make 82 ' ' ^ "|") :: !lines;
  lines :=
    ("|       " ^ quote ^ String.make (91 - String.length quote) ' ' ^ "|")
    :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  lines := ("|" ^ String.make 98 '=' ^ "|") :: !lines;
  let sprite =
    if pl_w then battle_camon (read_file "player_win.txt") 0
    else battle_camon (read_file "player_lose.txt") 0
  in
  let partner = battle_camon (read_file camon_fn) 0 in
  for i = 0 to 18 do
    lines := ("|" ^ List.nth sprite i ^ List.nth partner i ^ "|") :: !lines
  done;
  for i = 0 to 3 do
    lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines
  done;
  lines := ("|" ^ String.make 98 '=' ^ "|") :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  if pl_w then
    lines :=
      ("|       Press enter to proceed." ^ String.make 68 ' ' ^ "|") :: !lines
  else
    lines :=
      ("|       Press esc to restart :(" ^ String.make 68 ' ' ^ "|") :: !lines;
  let before_lines = 3 * ((98 - String.length name) / 4) in
  let after_lines = 98 - String.length name - before_lines in
  lines :=
    ("|"
    ^ String.make before_lines ' '
    ^ name
    ^ String.make after_lines ' '
    ^ "|")
    :: !lines;
  lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines;
  lines := ("|" ^ String.make 98 '=' ^ "|") :: !lines;
  for i = 0 to 2 do
    lines := ("|" ^ String.make 98 ' ' ^ "|") :: !lines
  done;
  lines := ("|" ^ String.make 98 '_' ^ "|") :: !lines;
  List.rev !lines

let victory =
  let open Yojson.Basic.Util in
  let lore = Yojson.Basic.from_file "data//lore.json" in
  lore |> member "victory" |> to_list |> List.map to_string

let map_instructions =
  let open Yojson.Basic.Util in
  let lore = Yojson.Basic.from_file "data//map_instructions.json" in
  lore |> member "map" |> to_list |> List.map to_string