(** Test Plans:

    - Trainerstate module: implemented using glass box testing in
      [trainer_tests]

    - Command module: implemented using glass box in [command_tests]

    - Display module: implemented using glass box in [display_tests]. This
      module also some functions that can only be tested manually from frontend.

    - Battle module: implemented using glass box testing in [battle_tests] and
      [wild_tests]. With this module being the main connection to frontend, it
      contains many helper functions and functions that involve randomness.
      Thus, we were unable to reach high code coverage for this module. Instead,
      we repeatedly played the game from the frontend and tested all the modules
      that it called to ensure correctness.

    - We added the list of extra test cases named [erroring_tests] that contain
      the special test cases where we found bugs when connecting with the
      frontend. We wanted to keep it separate to be able to have special
      attention on them and to test the full process of the battle, which is the
      immersion of multiple modules

    - This test approach demonstrate the correctness of the system because aside
      from some coverage that was tested on the frontend, our bisect report
      shows sufficient coverage *)

open OUnit2
open Camlventure
open Trainerstate
open Battle

(********************************************************************
   Helper functions 
 ********************************************************************)
(* From a2: *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(********************************************************************
   Load Data Files
 ********************************************************************)
let trainer_dir_prefix =
  "data" ^ Filename.dir_sep ^ "trainer" ^ Filename.dir_sep

let camon_dir_prefix = "data" ^ Filename.dir_sep ^ "camon" ^ Filename.dir_sep

(* trainers *)
let clarkson = Yojson.Basic.from_file (trainer_dir_prefix ^ "clarkson.json")
let doenges = Yojson.Basic.from_file (trainer_dir_prefix ^ "doenges.json")
let null = Yojson.Basic.from_file (trainer_dir_prefix ^ "null.json")
let base = Yojson.Basic.from_file (trainer_dir_prefix ^ "base.json")

(* camon *)
let unit1 = Yojson.Basic.from_file (camon_dir_prefix ^ "unit1.json")
let unit2 = Yojson.Basic.from_file (camon_dir_prefix ^ "unit2.json")
let unit3 = Yojson.Basic.from_file (camon_dir_prefix ^ "unit3.json")

(********************************************************************
   Trainerstate Tests
 ********************************************************************)

(** [make_test name input expected_output to_string] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with [input]. In
    the case of a failure, returns the string representation of the outputs
    using [to_string]*)
let make_test (name : string) (input : 'a) (expected_output : 'a)
    (to_string : 'a -> string) : test =
  name >:: fun _ -> assert_equal expected_output input ~printer:to_string

(** [na x] returns the empty string. can be used as a placeholder to_string
    function in [make_test] for types without string representations*)
let na x = ""

(** [attack_ids_exception name tr camon] constructs an OUnit test named [name]
    that asserts that [attack_ids tr camon] raises the exception
    [UnknownCamon camon]*)
let attack_ids_exception (name : string) (tr : Trainerstate.t) (camon : string)
    : test =
  name >:: fun _ ->
  assert_raises (UnknownCamon camon) (fun () -> attack_ids tr camon)

(** [attack_stats_exception name tr camon m exn] constructs an OUnit test named
    [name] that asserts that [attack_stats tr camon m] raises the exception
    [exn]*)
let attack_stats_exception (name : string) (tr : Trainerstate.t)
    (camon : string) (m : string) (exn : exn) : test =
  name >:: fun _ -> assert_raises exn (fun () -> attack_stats tr camon m)

(**[list_to_str lst] converts [lst] to a string by concatenating with ;;*)
let list_to_str = String.concat ";; "

(* initalize abstract trainer types *)
let tr_clarkson = from_json clarkson unit1
let tr_doenges = from_json doenges unit1
let null_init_1 () = from_json null unit1
let null_init_2 () = from_json null unit2
let null_init_3 () = from_json null unit3
let t1 = null_init_1 ()
let t2 = null_init_2 ()

let trainer_title_tests =
  [
    make_test "tr_clarkson's title is [Snarly Hacker Clarkson]"
      (trainer_title tr_clarkson)
      "Snarly Hacker Clarkson" Fun.id;
    make_test "trainer_doenges's title is [Snarly Apprentice Ryan Doenges]"
      (trainer_title tr_doenges) "Snarly Apprentice Ryan Doenges" Fun.id;
  ]

let camon_name_tests =
  [
    make_test "t1's camon is [unit1]" (active_camon t1) "unit1" Fun.id;
    make_test "t2's camon is [unit2]" (active_camon t2) "unit2" Fun.id;
  ]

let attack_id_tests =
  [
    make_test "t1's camon unit1 has moves [hit, miss]" (attack_ids t1 "unit1")
      [ "hit"; "miss" ] (pp_list pp_string);
    make_test "t2's camon unit2 has moves [ko]" (attack_ids t2 "unit2") [ "ko" ]
      (pp_list pp_string);
    attack_ids_exception "t1 does not have camel on its team" t1 "camel";
  ]

let attack_stats_tests =
  [
    make_test "t1's camon unit1 has move [hit] with 10 dmg and 100 accuracy"
      (attack_stats t1 "unit1" "hit")
      (10, 100) na;
    make_test "t2's camon unit2 has move [ko] with 100000 dmg and 50 accuracy"
      (attack_stats t2 "unit2" "ko")
      (100_000, 100) na;
    attack_stats_exception "unknown camon exception" t1 "camel" ""
      (UnknownCamon "camel");
    attack_stats_exception "unknown move exception" t1 "unit1" "ko"
      (UnknownMove "ko");
  ]

let _ =
  add_camon t1 unit2;
  add_camon t2 unit3;
  add_camon t2 unit1

let add_camon_tests =
  [
    make_test "t1's camon team is [unit2; unit1]" (camon_team t1)
      [ "unit2"; "unit1" ] (pp_list pp_string);
    make_test "t2's camon team is [unit1; unit3; unit2]" (camon_team t2)
      [ "unit1"; "unit3"; "unit2" ]
      (pp_list pp_string);
  ]

let op1 = null_init_2 ()
let op2 = null_init_3 ()

let use_tests =
  [
    make_test "tr1 attacks tr2 with unit1 using hit" (use "hit" t1 t2)
      (Success (Alive 40)) na;
    make_test "tr1 misses op1 with unit1 using miss" (use "miss" t1 op1)
      (Miss (Alive 50)) na;
    make_test "use fails when attempting to use ko with unit1" (use "ko" t1 t2)
      Illegal na;
    (* make_test "hp test before ko" (camon_hp op2 "unit3") 5 string_of_int; *)
    make_test "t1 defeats op2 with unit2 using hit" (use "hit" t1 op2)
      (Success Defeated) na;
    make_test "hp test unit2" (camon_hp t2 "unit2") 50 string_of_int;
  ]

let t3 = null_init_1 ()

let _ =
  add_camon t3 unit3;
  switch "unit3" t3

let _ =
  add_camon op2 unit1;
  switch "unit1" op2

let switch_camon_tests =
  [
    make_test "hp test test" (camon_hp op2 "unit3") 0 string_of_int;
    make_test "After switching, t3's active camon is [unit3]" (active_camon t3)
      "unit3" Fun.id;
    make_test "switch fails when t3 attempts to switch to unit2"
      (switch "unit2" t3) Illegal na;
    make_test "hp assurance" (camon_hp op2 "unit3") 0 string_of_int;
    make_test "switch fails when op2 attempts to switch to dead unit3"
      (switch "unit3" op2) Dead (fun x ->
        list_to_str (Display.switching x "unit3"));
  ]

let joanna = get_trainer (Player "Joanna")

let get_trainer_tests =
  [
    make_test "get_trainer test for Clarkson's camel"
      (active_camon (get_trainer Clarkson))
      "ocaml" Fun.id;
    make_test "get_trainer test for Clarkson's title"
      (trainer_title (get_trainer Clarkson))
      "Snarly Hacker Clarkson" Fun.id;
    make_test
      "initalize player trainerstate with name [Joanna] sets title to [Hero \
       Joanna]"
      (trainer_title joanna) "Hero Joanna" Fun.id;
    make_test "initalized player trainerstate has camon [jocalf]"
      (active_camon joanna) "jocalf" Fun.id;
    make_test "initalized player trainerstate has team [ducky; jocalf]"
      (camon_team joanna) [ "ducky"; "jocalf" ] (pp_list pp_string);
    make_test "initalized player trainerstate [jocalf] has 111"
      (camon_hp joanna (active_camon joanna))
      111 string_of_int;
  ]

let trainer_tests =
  List.flatten
    [
      trainer_title_tests;
      camon_name_tests;
      attack_id_tests;
      attack_stats_tests;
      add_camon_tests;
      switch_camon_tests;
      use_tests;
      get_trainer_tests;
    ]

(********************************************************************
   Command Tests
 ********************************************************************)

let command_tests =
  let open Command in
  [
    make_test "parse quit" (parse "quit") Quit na;
    make_test "parse Quit " (parse " Quit ") Quit na;
    make_test "parse use big tackle" (parse "use big tackle ")
      (Use "big tackle") na;
    make_test "parse view moves" (parse "view moves") (View Moves) na;
    make_test "parse view opponent" (parse "view opponent") (View Oppo) na;
    make_test "parse Switch three humped camel"
      (parse " Switch         three       Humped           camel       ")
      (Switch "three humped camel") na;
    ("Empty" >:: fun _ -> assert_raises Empty (fun () -> parse "          "));
    ("Malformed" >:: fun _ -> assert_raises Malformed (fun () -> parse "qquit"));
  ]

(********************************************************************
   Display Tests
 ********************************************************************)
let display_tr_1 = null_init_1 ()

let tr1_inital_camon_status =
  Trainerstate.Alive (camon_hp display_tr_1 (active_camon t1))

let display_tests =
  let open Display in
  [
    make_test "display legal switch"
      (switching Legal "sample camon")
      [ "Congrats! The new active camon is now changed to sample camon" ]
      list_to_str;
    make_test "display illegal switch"
      (switching Illegal "sample camon")
      [
        "Poopy!!! The camon does not exist in the current trainer's team. The \
         active camon is still the same";
        "Type 'View camons' to see the available camons on your team.";
      ]
      list_to_str;
    make_test "display switch to a defeated camon"
      (switching Dead "sample camon")
      [
        "Poopy!!! sample camon was found dead. Please switch to a camon that \
         is alive.";
      ]
      list_to_str;
    make_test "display a successful attack that results in an alive camon"
      (attack (Success tr1_inital_camon_status) "sample camon" "sample move")
      [
        "sample camon attempts to use sample move! ";
        "The attack HIT, the hp of the opponent's active camon is now reduced \
         to 100";
      ]
      list_to_str;
    make_test "display a successful attack that results in a defeated camon"
      (attack (Success Defeated) "sample camon" "sample move")
      [
        "sample camon attempts to use sample move! ";
        "Congrats, the attack HIT and DEFEATED the opponent's active camon";
      ]
      list_to_str;
    make_test "display a missed attack"
      (attack (Miss tr1_inital_camon_status) "sample camon" "sample move")
      [
        "sample camon attempts to use sample move! ";
        "Poopy!!! The attack MISSED, the hp of the opponent's active camon \
         remained at 100";
      ]
      list_to_str;
    make_test "display an illegal attack"
      (attack Illegal "sample camon" "sample move")
      [
        "sample camon attempts to use sample move! ";
        "Poopy!!! The move does not exist for the current active camon. Please \
         try again. Type 'View moves' to see available moves";
      ]
      list_to_str;
    make_test "display moves of tr1"
      (moves display_tr_1 (Trainerstate.active_camon display_tr_1))
      [ "hit (damage: 10, accuracy: 100)"; "miss (damage: 10, accuracy: 0)" ]
      list_to_str;
    make_test "display camons of tr1" (camons display_tr_1)
      [ "unit1 (hp: 100)" ] list_to_str;
    make_test "display status of tr1"
      (status display_tr_1 (Trainerstate.active_camon display_tr_1))
      [
        "Current camon name: unit1; hp: 100";
        "Available moves: ";
        "hit (damage: 10, accuracy: 100)";
        "miss (damage: 10, accuracy: 0)";
      ]
      list_to_str;
  ]

(********************************************************************
   Battle Tests
 ********************************************************************)
let pl = null_init_3 ()
let pl2 = null_init_3 ()
let _ = add_camon pl unit1
let pl3 = null_init_3 ()
let op = null_init_2 ()
let camel = Yojson.Basic.from_file (camon_dir_prefix ^ "camel.json")

let _ =
  let _ = add_camon pl3 unit1 in
  let _ = add_camon pl3 camel in
  let _ = switch "unit1" pl3 in
  let _ = use "ko" op pl3 in
  let _ = switch "unit3" pl3 in
  let _ = switch "camel" pl3 in
  ()

let battle = init_battle pl op false
let battle2 = init_battle pl2 op false
let battle3 = init_battle pl3 op false

let res_to_string r =
  match r with
  | Text t -> String.concat ";;" t
  | _ -> "empty"

let unit3_status =
  [
    "Current camon name: unit3; hp: 5";
    "Available moves: ";
    "chip (damage: 1, accuracy: 100)";
  ]

let camel_status =
  [
    "Current camon name: camel; hp: 111";
    "Available moves: ";
    "syntaxle (damage: 40, accuracy: 100)";
    "polymorph (damage: 65, accuracy: 100)";
    "click (damage: 70, accuracy: 70)";
    "dune build (damage: 70, accuracy: 100)";
  ]

let battle_tests =
  [
    make_test "empty string" (pl_rnd "" battle)
      (Battle.Text
         [ "What was that?"; "Type \"Help\" to see all available commands" ])
      res_to_string;
    make_test "quit" (pl_rnd "  quit " battle)
      (Battle.Text [ "You can't run from a trainer battle!" ]) na;
    make_test "battle help" (pl_rnd "  help  " battle)
      (Battle.Text
         [
           "Try to get your opponents hp to zero using one of your moves! type \
            \"Use <move_name>\" to use the move specified.";
           "To switch your active camon with one on your team type \"Switch \
            <camon_name>\"";
           "To see more information type \"View\" followed by whatever you are \
            interested in (team, status, moves, camon)";
         ])
      res_to_string;
    make_test "battle view team"
      (pl_rnd "view team " battle)
      (Battle.Text [ "unit1 (hp: 100)"; "unit3 (hp: 5)" ])
      res_to_string;
    make_test "battle view moves"
      (pl_rnd "view moves " battle2)
      (Battle.Text [ "chip (damage: 1, accuracy: 100)" ]) res_to_string;
    make_test "battle view status unit3"
      (pl_rnd "view status " battle2)
      (Battle.Text unit3_status) res_to_string;
    make_test "battle default view" (pl_rnd "view " battle2)
      (Battle.Text unit3_status) res_to_string;
    make_test "battle view camon unit3"
      (pl_rnd "view camon unit3 " battle)
      (Battle.Text unit3_status) res_to_string;
    make_test "battle view camon that is not active but is on the team"
      (pl_rnd "view camon unit1 " battle3)
      (Battle.Text
         [
           "Current camon name: unit1; hp: 0";
           "Available moves: ";
           "hit (damage: 10, accuracy: 100)";
           "miss (damage: 10, accuracy: 0)";
         ])
      res_to_string;
    make_test "view non-existent camon"
      (pl_rnd "view camon unit1 " battle2)
      (Battle.Text
         [ "Poopy! You made a boo boo! unit1 does not exist in your team" ])
      res_to_string;
    make_test "switch to legal"
      (pl_rnd "switch unit1 " battle)
      (Battle.Text [ "Congrats! The new active camon is now changed to unit1" ])
      res_to_string;
    make_test "switch to dead"
      (pl_rnd "switch unit1 " battle3)
      (Battle.Text
         [
           "Poopy!!! unit1 was found dead. Please switch to a camon that is \
            alive.";
         ])
      res_to_string;
    make_test "switch non-existence"
      (pl_rnd "switch oo" battle3)
      (Battle.Text
         [
           "Poopy!!! The camon does not exist in the current trainer's team. \
            The active camon is still the same";
           "Type 'View camons' to see the available camons on your team.";
         ])
      res_to_string;
  ]

(********************************************************************
   Wild Battle Tests
 ********************************************************************)

let wild = init_battle pl op true

let wild_tests =
  [
    make_test "run away from trainer battle" (pl_rnd "quit" wild)
      (Battle.BattleR { s = Win; text = [ "Got away safely!" ] })
      res_to_string;
  ]

(********************************************************************
   Test Erroring Functions found from frontend
 ********************************************************************)
(*TEST VIEWING A CAMON THAT IS ON TEAM BUT NOT ACTIVE TEST THE STARTING HP WHEN
  TYPE "USE"*)
let erroring_tests =
  [
    make_test "Specific error: view camon camel"
      (pl_rnd "view camon Camel" battle3)
      (Battle.Text camel_status) res_to_string;
    make_test "battle view team"
      (pl_rnd "view team " battle3)
      (Battle.Text [ "camel (hp: 111)"; "unit1 (hp: 0)"; "unit3 (hp: 5)" ])
      res_to_string;
    make_test "battle view status camel"
      (pl_rnd "view status " battle3)
      (Battle.Text camel_status) res_to_string;
  ]

(********************************************************************
   Build Test Suite
 ********************************************************************)
let suite =
  "test suite for camlventure"
  >::: List.flatten
         [
           trainer_tests;
           command_tests;
           battle_tests;
           wild_tests;
           display_tests;
           erroring_tests;
         ]

let _ = run_test_tt_main suite
