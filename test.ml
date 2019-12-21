open OUnit2
open Board
open Game
open Command
open Str
open Yojson
open Word
open Allwords

(*  test.ml is used to test the validity of functions Board, Game
    Command, Word, and Allwords. These are modules that are used either
    by other of the listed modules or in Main.

    White-box testing was used in developing the code as to test every 
    possible test case and predict any possible errors that could occur.
    Consideration here was also taken for helper functions that are not present
    in the .mli as to provide complete coverage. As helper functions were 
    called by functions present in the mli and tested here, it was not
    necessary to test helper functions. 

    Due to the degree of randomness present in the game, aspects of the 
    game here were hard-coded for testing here. 

    Completely random features that were not hardcoded and all features of
    Main were tested manually by using "make play" and seeking to perform
    all of the aforementioned features and commands mentioned in INSTALL.txt

    This approach provided complete coverage in making sure the repl 
    performed as intended and that the repl called upon functions
    that performed their inteded effect. 

    One thing to note is that "make test" does take about a minute to run
    due to the large number of computationally-extensive functions that
    are needed to initialize a game. *)

(** [cmp_lists lst1 lst2] compares two lists to see whether
    they are equivalent in that they contain the same elements and have the
    same length *)
let cmp_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  uniq1 = uniq2

(** [pp_endgame_command c] pretty-prints command [c] *)
let pp_endgame_command c =
  match c with
  | Allwords -> "Allwords"
  | Newgame -> "Newgame"
  | EndgameQuit -> "EndgameQuit"
  | Score -> "Score"

(** [pp_passive_command c] pretty-prints command [c] *)
let pp_passive_command c = 
  match c with 
  | Start str -> "Start \"" ^ str ^ "\""
  | PassiveQuit -> "PassiveQuit"

(** [pp_active_command c] pretty-prints command [c] *)
let pp_active_command c =
  match c with 
  | ActiveQuit -> "ActiveQuit"
  | End -> "End"
  | Words -> "Words"
  | Hint -> "Hint"
  | Word str -> "Word \"" ^ str ^ "\""
  | Score -> "Score"

(** [pp_char c] pretty-prints char [c] *)
let pp_char c = "\'" ^ (String.make 1 c) ^ "\'"

(** [pp_string s] pretty-prints string [s]. Taken from CS3110-A2 *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. Taken from CS3110-A2 *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(** [pp_block b] pretty-prints block [b] *)
let pp_block b = 
  "letter: " ^ pp_char b.letter ^ 
  "\nindex: " ^ string_of_int b.index ^ 
  "\nneighbors: " ^ (pp_list string_of_int b.neighbors)

(** [pp_player b] pretty-prints player [p] *)
let pp_player p = 
  "name: " ^ pp_string p.name ^ 
  "\nwords: " ^ pp_list pp_string p.words ^ 
  "\npoints: " ^ string_of_int p.points ^
  "\nhas_turn: " ^ string_of_bool p.has_turn

(** [sample_board] is a Boggle board of type Board.t that is hard-coded
    for use in testing*)
let sample_board = 
  let ltrs = [(1,'R'); (2,'B'); (3,'Y'); (4,'Q'); (5,'U'); (6,'O'); (7,'F');
              (8,'O'); (9,'R'); (10,'E'); (11,'N'); (12,'O'); (13,'Y');
              (14,'I'); (15,'N'); (16,'R')] in 
  let rec ltrs_to_blocks ltrs acc =
    match ltrs with 
    | [] -> acc
    | (idx, char)::t ->
      ltrs_to_blocks t ({letter = char;
                         index = idx;
                         neighbors = neighbor_indices idx}::acc) in
  {blocks = (ltrs_to_blocks ltrs []) @ []}

(** [sample_game player_names] is a Bogggle game of type Game.t that uses a
    hard-coded board to initialize the game with players [player_names] *)
let sample_game player_names = 
  init_game sample_board player_names

(** [make_get_index_test] constructs an OUnit test named [name] that asserts
     the quality of [expected_output] with [get_index board block]*)
let make_get_index_test 
    (name : string)
    (block_input : Board.block)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (get_index block_input)
        ~printer:string_of_int)

let board_blocks = get_blocks sample_board 
let get_index_tests = [
  make_get_index_test "return index of first block, 1" 
    (List.nth board_blocks 15) 1; 
  make_get_index_test "return index of the last block, 16"
    (List.nth board_blocks 0) 16;
]

(** [make_neighbors_indices_test ] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [neighbor_indices index] *)
let make_neighbor_indices_test 
    (name : string)
    (index_input : int) 
    (expected_output : int list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (neighbor_indices index_input)
        ~printer:(pp_list string_of_int))

let neighbor_indices_tests = [
  make_neighbor_indices_test "test neighbors of corner, index 1"
    1 [2; 5; 6];
  make_neighbor_indices_test "test neighbors of side, index 10"
    10 [5; 6; 7; 9; 11; 13; 14; 15];
  make_neighbor_indices_test "test neighbor of center, index 15"
    15 [10; 11; 12; 14; 16];
]

(** [make_get_letter_test] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [get_letter board_
    input block_input] *)
let make_get_letter_test
    (name : string)
    (board_input : Board.t)
    (block_input : Board.block)
    (expected_output : char) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_letter board_input block_input)
        ~printer: pp_char)

let board_blocks = get_blocks sample_board 
let get_letter_tests = [
  make_get_letter_test "get letter of last block, which should be R"
    sample_board (List.nth board_blocks 1) 'R';
  make_get_letter_test "get letters of second-to-last block, which should be N"
    sample_board (List.nth board_blocks 2) 'N';
]

(** [make_get_letter_from_index_ test] constructs an OUnit test named [name] 
    that asserts the quality of [expected_output] with [get_letter board_
    input block_input] *)
let make_get_letter_from_index_test
    (name : string)
    (board_input : Board.t)
    (index_input : int)
    (expected_output : char) : test =
  name >:: (fun _ ->
      assert_equal expected_output ~printer:pp_char 
        (get_letter_from_index board_input index_input))

let board_blocks = get_blocks sample_board 
let get_letter_from_index_tests = [
  make_get_letter_from_index_test "get letter of index 0, which should be R"
    sample_board 0 'R';
  make_get_letter_from_index_test "get letter of index 1, which should be N"
    sample_board 1 'B';
]

(** [make_letter_to_block_test] constructs an OUnit test named [name] that
     asserts the quality of [expected_output] with [letter_to_block board_input
     letter_input]*)
let make_letter_to_block_test
    (name : string) 
    (board_input : Board.t)
    (letter_input : char)
    (expected_output : Board.block list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (letter_to_block board_input letter_input)
        ~printer:(pp_list pp_block))

let make_letter_to_block_tests = [
  make_letter_to_block_test "test letter Y" sample_board 'Y' 
    [{letter = 'Y'; index = 13; neighbors = [9; 10; 14]};
     {letter = 'Y'; index = 3; neighbors = [2; 4; 6; 7; 8]};
    ];
  make_letter_to_block_test "test letter R" sample_board 'R'
    [{letter = 'R'; index = 16; neighbors = [11; 12; 15]};
     {letter = 'R'; index = 9; neighbors = [5; 6; 10; 13; 14]};
     {letter = 'R'; index = 1; neighbors = [2; 5; 6]};
    ];
  make_letter_to_block_test "test letter O" sample_board 'O'
    [{letter = 'O'; index = 12; neighbors = [7; 8; 11; 15; 16]};
     {letter = 'O'; index = 8; neighbors = [3; 4; 7; 11; 12]};
     {letter = 'O'; index = 6; neighbors = [1; 2; 3; 5; 7; 9; 10; 11]};
    ];
  make_letter_to_block_test "test letter N" sample_board 'N'
    [{letter = 'N'; index = 15; neighbors = [10; 11; 12; 14; 16]};
     {letter = 'N'; index = 11; neighbors = [6; 7; 8; 10; 12; 14; 15; 16]};
    ];
  make_letter_to_block_test "test letter E" sample_board 'E'
    [{letter = 'E'; index = 10; neighbors = [5; 6; 7; 9; 11; 13; 14; 15]};
    ];  
  make_letter_to_block_test "test letter A" sample_board 'A'
    []
]

(** [make_board_to_letter_list_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [board_to_letter_list
    board_input]*)
let make_board_to_letter_list_test
    (name : string)
    (board_input : Board.t)
    (expected_output : char list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (board_to_letter_list board_input)
        ~printer:(pp_list pp_char))

let board_to_letter_list_tests = [ 
  make_board_to_letter_list_test "test sample board which has letters
  RBYQUOFORENOYINR" sample_board ['R'; 'B'; 'Y'; 'Q'; 'U'; 'O'; 'F'; 'O';
                                  'R'; 'E'; 'N'; 'O'; 'Y'; 'I'; 'N'; 'R'];
]

(** [make_legal_word_in_board_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [legal_word_in_board
    word_input board_input] *)
let make_legal_word_in_board_test 
    (name : string)
    (word_input : string)
    (board_input : Board.t)
    (expected_output : bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (legal_word_in_board word_input board_input)
        ~printer: string_of_bool)

let legal_word_in_board_tests = [
  make_legal_word_in_board_test "qoor - occurs in board, not valid
  english word, but is valid in board" "qoor"  sample_board true;
  make_legal_word_in_board_test "no - occurs in board, too short,
  but is valid in board" "no" sample_board true;
  make_legal_word_in_board_test "ruby - occurs in english language and 
  in board" "ruby" sample_board true;
  make_legal_word_in_board_test "RuBy - testing capital letters" "RuBy"
    sample_board true;
  make_legal_word_in_board_test "hello, illegal word in board" "hello" 
    sample_board false;
]

(** [make_is_word_test] consturcts an OUnit test named [name] that asserts
    the quality of [expected_output] with [word string_input]*)
let make_is_word_test 
    (name : string)
    (word_input : string)
    (expected_output : bool) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (is_word word_input)
        ~printer: string_of_bool)

let is_word_tests =[
  make_is_word_test "hello" "hello" true;
  make_is_word_test "Hello" "Hello" true;
  make_is_word_test "hello " "hello " false;
  make_is_word_test "hello9" "hello9" false;
]

(** [make_endgame_parse_test] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [endgame_parse str_input]*)
let make_endgame_parse_test 
    (name : string)
    (str_input : string)
    (expected_output : Command.endgame_command) : test =
  name >:: (fun _ ->
      assert_equal expected_output (endgame_parse str_input)
        ~printer: pp_endgame_command)  

let endgame_parse_tests = [
  make_endgame_parse_test "quit" "quit" EndgameQuit;
  make_endgame_parse_test "score" "score" Score;
  make_endgame_parse_test "rematch" "rematch" Newgame;
  make_endgame_parse_test "words" "words" Allwords;
]

(** [make_endgame_parse_text_exn] constructs an OUnit test named [name] that
    asserts that the exception [expected_output] is equal to that
    raised in [endgame_parse str_input*)
let make_endgame_parse_test_exn
    (name : string)
    (str_input : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> endgame_parse str_input))

let endgame_parse_exn_tests = [
  make_endgame_parse_test_exn "quit hello" "quit hell" Malformed;
  make_endgame_parse_test_exn "score hello" "score hello" Malformed;
  make_endgame_parse_test_exn "new hello" "new hello" Malformed;
  make_endgame_parse_test_exn "words hello" "words hello" Malformed;
  make_endgame_parse_test_exn "empty string" "" Empty;
  make_endgame_parse_test_exn "string of spaces" "    " Empty;
]

(** [make_passive_parse_test] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [passive_parse str_input]*)
let make_passive_parse_test 
    (name : string)
    (str_input : string)
    (expected_output : Command.passive_command) : test =
  name >:: (fun _ ->
      assert_equal expected_output (passive_parse str_input)
        ~printer: pp_passive_command)  

let passive_parse_tests = [
  make_passive_parse_test "quit" "quit" PassiveQuit;
  make_passive_parse_test "start Max" "start Max" (Start "Max");
]

(** [make_passive_parse_text_exn] constructs an OUnit test named [name] that
    asserts that the exception [expected_output] is equal to that
    raised in [passive_parse str_input*)
let make_passive_parse_test_exn
    (name : string)
    (str_input : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> passive_parse str_input))

let passive_parse_exn_tests = [
  make_passive_parse_test_exn "empty string" "" Empty;
  make_passive_parse_test_exn "string of spaces" "    " Empty;
  make_passive_parse_test_exn "quit Ronnie" "quit Ronnie" Malformed;
  make_passive_parse_test_exn "start" "start" Malformed;
  make_passive_parse_test_exn "start Max Ronnie" "start Max Ronnie" Malformed;
]

(** [make_active_parse_test] constructs an OUnit test named [name] that
    asserts the quality of [expected_output] with [active_parse str_input] *)
let make_active_parse_test
    (name : string) 
    (str_input : string)
    (expected_output : Command.active_command) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (active_parse str_input)
        ~printer: pp_active_command)

let active_parse_tests = [
  make_active_parse_test "quit" "quit" ActiveQuit;
  make_active_parse_test "end" "end" End;
  make_active_parse_test "words" "words" Words;
  make_active_parse_test "score" "score" Score;
  make_active_parse_test "word hello" "word hello" (Word "hello");
  make_active_parse_test "hint" "hint" Hint;
]

(** [make_active_parse_text_exn] constructs an OUnit test named [name] that
    asserts that the exception [expected_output] is equal to that
    raised in [active_parse str_input*)
let make_active_parse_test_exn
    (name : string)
    (str_input : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> active_parse str_input))

let active_parse_exn_tests = [
  make_active_parse_test_exn "empty string" "" Empty;
  make_active_parse_test_exn "string of spaces" "    " Empty;
  make_active_parse_test_exn "quit Ronnie" "quit Ronnie" Malformed;
  make_active_parse_test_exn "end Ronnie" "end Ronnie" Malformed;
  make_active_parse_test_exn "words Ronnie" "words Ronnie" Malformed;
  make_active_parse_test_exn "score Ronnie" "score Ronnie" Malformed;
  make_active_parse_test_exn "word" "word" Malformed;
  make_active_parse_test_exn "word hello help" "word hello help" Malformed;
  make_active_parse_test_exn "hint hello" "hint hello" Malformed;
]

(** [make_get_player_names_test] constructs an OUnit test named [name] that
    asserts the quality of [expected_output] with [get_player_name
    game_input]*)
let make_get_player_names_test
    (name : string)
    (game_input : Game.t)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_player_names game_input) 
        ~cmp: cmp_lists ~printer:(pp_list pp_string))

let no_players = sample_game []  
let one_player = sample_game ["Ronnie"] 
let two_players = sample_game ["Ronnie"; "Max"] 
let get_player_names_tests = [ 
  make_get_player_names_test "no players" no_players [];
  make_get_player_names_test "Ronnie" one_player ["Ronnie"];
  make_get_player_names_test "Ronnie" two_players ["Ronnie"; "Max"];
]

(** [make_get_words_of_player_test] constructs an OUnit test named [name] that
    asserts the quality of [expected_output] with [get_words_of_player
    game_input player_input] *)
let make_get_words_of_player_test
    (name : string)
    (game_input : Game.t)
    (player_name_input : string) 
    (expected_output : string list) : test =
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_lists ~printer:(pp_list pp_string)
        expected_output (get_words_of_player game_input player_name_input))

let no_words = sample_game ["Ronnie"]
let one_word = add_word no_words "Ronnie" "iron"
let two_words = add_word one_word "Ronnie" "bury"
let two_players_no_word = sample_game ["Ronnie"; "Max"]
let two_players_one_word = add_word two_players_no_word "Ronnie" "iron"
let two_players_two_words = add_word two_players_one_word "Max" "bury"
let three_players_no_word = sample_game ["Ronnie"; "Max"; "John"]
let three_players_one_word = add_word three_players_no_word "Ronnie" "iron"
let three_players_two_words = add_word three_players_one_word "Max" "bury"
let three_players_three_words = add_word three_players_two_words "John" "neon"
let three_players_four_words = add_word three_players_three_words "Ronnie" 
    "nine"

let get_words_of_player_tests = [
  make_get_words_of_player_test "empty, Ronnie" no_words "Ronnie" [];
  make_get_words_of_player_test "iron, Ronnie" one_word "Ronnie" ["iron"];
  make_get_words_of_player_test "iron bury, Ronnie" two_words "Ronnie"
    ["iron"; "bury"];
  make_get_words_of_player_test "empty, Ronnie Max" two_players_no_word
    "Ronnie" [];
  make_get_words_of_player_test "empty, Ronnie Max" two_players_no_word
    "Max" [];
  make_get_words_of_player_test "iron, Ronnie [], Max, testing Ronnie" 
    two_players_one_word "Ronnie" ["iron"];
  make_get_words_of_player_test  "iron, Ronnie [], Max, testing Max"
    two_players_one_word "Max" [];
  make_get_words_of_player_test "iron, Ronnie and bury, Max, testing Ronnie"
    two_players_two_words "Ronnie" ["iron"];
  make_get_words_of_player_test "iron, Ronnie and bury, Max, testing Max"
    two_players_two_words "Max" ["bury"];
  make_get_words_of_player_test "iron, Ronnie [], Max [], John, testing Ronnie"
    three_players_one_word "Ronnie" ["iron"];
  make_get_words_of_player_test "iron, Ronnie [], Max [], John, testing Max"
    three_players_one_word "Max" [];  
  make_get_words_of_player_test "iron, Ronnie [], Max [], John, testing John"
    three_players_one_word "John" [];
  make_get_words_of_player_test "iron, Ronnie / bury, Max / neon, John,
   testing John"
    three_players_three_words "John" ["neon"];
  make_get_words_of_player_test "iron, nine, Ronnie / bury, Max / neon,  John, 
  testing Ronnie"
    three_players_four_words "Ronnie" ["iron"; "nine"];
]

(** [make_get_score_of_player_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with 
    [get_score_of_player game_input player_name_input] *)
let make_get_score_of_player_test 
    (name : string)
    (game_input : Game.t)
    (player_name : string)
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_score_of_player game_input
                                      player_name) ~printer:(string_of_int))

let get_score_of_player_tests = [
  make_get_score_of_player_test "return 0 for Ronnie in new game"
    (sample_game ["Ronnie"; "Max"]) "Ronnie" 0;
] 

(** [make_set_score_of_player_test] constructs an OUnit test named [name]
    that asserts the quality of [score_input] with the score of the player with
    [player_name] after performing [set_score_of_player_test game_input
    player_name_input score_input]*)
let make_set_score_of_player_test
    (name : string)
    (game_input : Game.t)
    (player_name : string)
    (score_input : int) : test =
  name >:: (fun _ ->
      let new_game = 
        set_score_of_player game_input player_name score_input in 
      let score_output = get_score_of_player new_game player_name in 
      assert_equal score_input score_output
        ~printer:(string_of_int))

let set_score_of_player_tests = [
  make_set_score_of_player_test "set score of 50 for Ronnie from new_game" 
    (sample_game ["Ronnie"; "Max"]) "Ronnie" 50
]

(** [make_is_english_word_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with
    [is_english_word word_input]*)
let make_is_english_word_test 
    (name : string)
    (word_input : string)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Word.is_english_word word_input)
        ~printer:string_of_bool)

let is_english_word_tests = [ 
  make_is_english_word_test "test hello in english dict" "hello" true;
  make_is_english_word_test "test fadsss in english dict" "fadsss" false;
  make_is_english_word_test "test capital letters" "HELlo" true;
]

(** [make_turn_to_hint_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [turn_to_hint word]*)
let make_turn_to_hint_test
    (name : string)
    (word_input : string)
    (expected_output : string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Word.turn_to_hint word_input)
        ~printer:pp_string)

let turn_to_hint_tests = [
  make_turn_to_hint_test "hid - 3 letters" "hid" "hi*";
  make_turn_to_hint_test "hide - 4 letters" "hide" "hi**";
  make_turn_to_hint_test "hides - 5 letters" "hides" "hi***";
  make_turn_to_hint_test "hidden - 6 letters" "hidden" "hid***";
  make_turn_to_hint_test "doritos - 7 letters" "doritos" "dor****";
  make_turn_to_hint_test "creative - 8 letters" "creative" "crea****";
  make_turn_to_hint_test "helplessness - 12 letters" "helplessness"
    "help********";
]

(** [make_who_won_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with [make_who_won]*)
let make_who_won_test
    (name: string)
    (players: Game.player list)
    (expected_output: Game.player) : test =
  name >:: (fun _ ->
      assert_equal expected_output (who_won players) ~printer: pp_player)

let who_won_tests = [
  make_who_won_test "Ronnie has 7 points and John has 5 points"
    [{name = "Ronnie"; words = []; points = 7; has_turn= true};
     {name = "John"; words = []; points = 5; has_turn= true};] 
    {name = "Ronnie"; words = []; points = 7; has_turn= true};
  make_who_won_test "Ronnie has 7 points, John has 5 points and Max has 3 
  points"
    [{name = "Ronnie"; words = []; points = 7; has_turn= true};
     {name = "John"; words = []; points = 5; has_turn= true};
     {name = "Max"; words = []; points = 3; has_turn= true};] 
    {name = "Ronnie"; words = []; points = 7; has_turn= true};
  make_who_won_test "Ronnie has 7 points, John has 5 points and Max also has 5
   points"
    [{name = "Ronnie"; words = []; points = 7; has_turn= true};
     {name = "John"; words = []; points = 5; has_turn= true};
     {name = "Max"; words = []; points = 5; has_turn= true};] 
    {name = "Ronnie"; words = []; points = 7; has_turn= true};
  make_who_won_test "Ronnie has 7 points, John has 7 points and Max has 5
   points; 
  first player with the highest points wins if there are multiple winners"
    [{name = "Ronnie"; words = []; points = 7; has_turn= true};
     {name = "John"; words = []; points = 7; has_turn= true};
     {name = "Max"; words = []; points = 5; has_turn= true};] 
    {name = "Ronnie"; words = []; points = 7; has_turn= true};
]

(** [make_player_score_test] constructs an OUnit test named [name] that
    asserts the quality of [expected_output] with [player_score 
    player_words_input other_words_input] *)
let make_player_score_test 
    (name : string)
    (player_words_input : string list) 
    (other_words_input : string list) 
    (expected_output : int) : test =
  name >:: (fun _ ->
      assert_equal expected_output  
        (player_score player_words_input other_words_input)
        ~printer:string_of_int)

let player_score_tests = [
  make_player_score_test "two empty sets: [] []" [] [] 0;
  make_player_score_test "other_players has words: [] [\"hello\"]"
    [] ["hello"] 0;
  make_player_score_test "player_words has one word, other_players has none 
  [\"hello\"] []" ["hello"] [] 2;
  make_player_score_test "player_words has two words, other_players has none 
  [\"hello\"; \"hi\"] []" ["hello"; "hi"] [] 2;
  make_player_score_test "player_words has words of different values, 
  other_players has none [\"hello\"; \"help\"] []" ["hello"; "help"] [] 3;
  make_player_score_test "player_words and other_words both have same word 
  [\"hello\"] [\"hello\"]" ["hello"] ["hello"] 0;
  make_player_score_test "player_words and other_words both have words, 
  but are different [\"hello\"] [\"help\"]" ["hello"] ["help"] 2;
  make_player_score_test "player_words testing word of length 3, hid, which 
  should be one point" ["hid"] [] 1;
  make_player_score_test "player_words testing word of length 4, hide, which
  should be one point" ["hide"] [] 1;
  make_player_score_test "player_words testing word of length 5, hides, which
  should be two points" ["hides"] [] 2;
  make_player_score_test "player_words testing word of length 6, hidden, which
  should be three points" ["hidden"] [] 3;
  make_player_score_test "player_words testing word of length 7, seating, which
  should be four points" ["seating"] [] 4;
  make_player_score_test "player_words testing word of greater than
  length 7, creative, which should be 11 points" ["creative"] [] 11;
]

(** [make_search_board_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with
    [search_board board_input depth_input] *)
let make_search_board_test
    (name : string)
    (board_input : Board.t)
    (depth_input : int)
    (expected_output : string list) : test =
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_lists ~printer:(pp_list pp_string)
        expected_output (Allwords.search_board board_input depth_input)
    )

let game = sample_game []
let search_board_tests = [
  make_search_board_test "depth 1, board dice_sample, depth 1" game.board 1
    ["R"; "B"; "Y"; "Q"; "U"; "O"; "F"; "O"; "R"; "E"; "N"; "O"; "Y"; "I"; "N";
     "R"];
  make_search_board_test "depth 2, board dice_sample, depth 2" game.board 2
    ["RB"; "RU"; "RO"; "R"; "BR"; "BY"; "BU"; "BO"; "BF"; "B"; "YB"; "YQ"; "YO";
     "YF"; "YO"; "Y"; "QY"; "QF"; "QO"; "Q"; "UR"; "UB"; "UO"; "UR"; "UE"; "U";
     "OR"; "OB"; "OY"; "OU"; "OF"; "OR"; "OE"; "ON"; "O"; "FB"; "FY"; "FQ"; 
     "FO"; "FO"; "FE"; "FN"; "FO"; "F"; "OY"; "OQ"; "OF"; "ON"; "OO"; "O"; 
     "RU"; "RO"; "RE"; "RY"; "RI"; "R"; "EU"; "EO"; "EF"; "ER"; "EN"; "EY"; 
     "EI"; "EN"; "E"; "NO"; "NF"; "NO"; "NE"; "NO"; "NI"; "NN"; "NR"; "N"; "OF";
     "OO"; "ON"; "ON"; "OR"; "O"; "YR"; "YE"; "YI"; "Y"; "IR"; "IE"; "IN"; "IY";
     "IN"; "I"; "NE"; "NN"; "NO"; "NI"; "NR"; "N"; "RN"; "RO"; "RN"; "R"];
]

(** [make_all_valid_words_test] constructs an OUnit test named [name]
    that asserts the quality of [expected_output] with
    [all_valid_words board_input depth_input] *)
let make_valid_words_test
    (name : string)
    (board_input : Board.t)
    (depth_input : int)
    (expected_output : string list) : test =
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_lists ~printer:(pp_list pp_string)
        expected_output (Allwords.valid_words board_input depth_input)
    )

let valid_words_test = [
  (*similar test to of length 15, as no words exceed length 8
    on this given board *)
  make_valid_words_test "tests words up to depth 8" game.board 8
    ["bon"; "bone"; "boner"; "bonn"; "bonne"; "bono"; "bore"; "boy"; "buen"; 
     "bueno"; "buon"; "buoy"; "bur"; "burin"; "bury"; "ein"; "eon"; "fen"; 
     "fer"; "feu"; "fey"; "fob"; "foe"; "for"; "fore"; "forenoon"; "fou"; 
     "four"; "infer"; "inn"; "inner"; "ire"; "iron"; "neo"; "neon"; "nfor"; 
     "nine"; "nob"; "non"; "none"; "noon"; "nor"; "norn"; "oer"; "ofo"; "one";
     "onine"; "onor"; "orb"; "ore"; "our"; "rei"; "rein"; "reno"; "rien";
     "rob"; "roe"; "ron"; "roof"; "roofer"; "rou"; "roue"; "rub"; "ruby"; 
     "rue"; "rye"; "urine"; "yen"; "yon"; "yore"; "you"; "your"; "youre"]
]

let suite =
  "test suite for final project"  >::: List.flatten [
    get_index_tests;
    neighbor_indices_tests;
    get_letter_tests;
    get_letter_from_index_tests;
    board_to_letter_list_tests;
    make_letter_to_block_tests;
    legal_word_in_board_tests;
    is_word_tests;
    endgame_parse_tests;
    endgame_parse_exn_tests;
    passive_parse_tests;
    passive_parse_exn_tests;
    active_parse_tests;
    active_parse_exn_tests;
    get_player_names_tests;
    get_words_of_player_tests;
    turn_to_hint_tests;
    get_score_of_player_tests;
    set_score_of_player_tests;
    is_english_word_tests;
    player_score_tests;
    search_board_tests;
    valid_words_test;
    who_won_tests;
  ]

let _ = run_test_tt_main suite