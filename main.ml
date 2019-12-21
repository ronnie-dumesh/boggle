open Board
open Game
open Command
open Word
open Allwords

(** [print_board board] is a formatted string of the board [board] that the game
    can print when a player's turn is called. *)
let print_board board = 
  print_string "       -----------\n";
  for i = 0 to 15 do
    if i mod 4 = 0 
    then (print_string "       | ";
          print_string
            (String.make 1 (get_letter_from_index board (i))^" "))
    else if i mod 4 = 3
    then (print_string
            (String.make 1 (get_letter_from_index board (i)));
          print_string " |\n")
    else print_string (String.make 1 (get_letter_from_index board (i))^
                       " ") done;
  print_string "       -----------\n"

(** [string_of_time t] is a formatted string of the time [t] where [t] is a time
    in seconds. *)
let string_of_time t = 
  let int_mins = t/.60. |> int_of_float in
  let int_sec = t |> int_of_float in
  let string_sec = int_sec mod 60 |> string_of_int in
  let string_min = int_mins |> string_of_int in
  match string_min, string_sec with
  | "0", "1" -> "1 second"
  | "1", "1" -> "1 minute and 1 second"
  | "1", "0" -> "1 minute"
  | "0", sec -> sec^" seconds"
  | "1", sec -> "1 minute and "^sec^" seconds"
  | min, "0" -> min^" minutes"
  | min, "1" -> min^" minutes and 1 second"
  | min, sec -> min^" minutes and "^sec^" seconds"

(** [clear_screen ()] clears the command prompt screen of any old print
    statements. *)
let clear_screen () = ignore (Sys.command "clear")

(** [quit ()] clears the command promp screen of any old print statements,
    prints a thank you message, and ends the game. *)
let quit () = 
  clear_screen ();
  print_endline "\nThanks for playing!\n"; 
  exit 0

(** [repl game error_msg] starts the main state of the game [game] and handles
    any error message [error_msg] passed along. *)
let rec repl game error_msg= 
  if no_turns_left game.players
  then final_score_calc game
  else print_string "";
  clear_screen ();
  print_endline ("\nPlayer(s) with turn remaining: "^
                 (get_players_left game
                  |> List.sort compare
                  |> List.rev
                  |> string_of_list "")^"."); 
  print_endline ("\nType \"start [player name]\" to start the round for the"^
                 " player.\n");
  if error_msg <> ""
  then print_string (error_msg^"\n> ";)
  else print_string "> ";
  match Command.passive_parse(read_line()) with
  | Start player_name -> (player_turn true game player_name (Unix.time ()+.180.)
                            "");
  | PassiveQuit -> quit ()
  | exception Malformed -> repl game "That is not a legal command.\n"
  | exception Empty -> repl game "No command has been input.\n"

(** [final_score_calc] calculates and updates everybody's score in [game] *)
and final_score_calc game =
  update_final_scores game 
  |> end_game

(** [end_game game] is the state of the game that ends the game [game]. *)
and end_game game = 
  clear_screen ();
  print_endline 
    ("All players have completed their turn and the game has finished! 
    \nEnter \"score\" to see everybody's score, \"words\" to see all possible"^
     " words in the \nboard, \"rematch\" to rematch (your old score will be added to"^
     " your score \nin the rematch), or \"quit\" to quit.\n");
  print_string ">";
  match Command.endgame_parse(read_line()) with 
  | Score ->
    clear_screen (); 
    print_words game game.players;
    print_scores game game.players;
    print_who_won game game.players;
    print_rankings game;
    end_game_transition game "" 
  | Allwords ->
    clear_screen (); 
    print_all_words game;
    end_game_transition game "";
  | Newgame -> rematch_game game 
  | EndgameQuit -> quit (); 
  | exception Malformed -> 
    end_game_transition game "That is not a legal command.\n"
  | exception Empty -> 
    end_game_transition game "No command has been input.\n"

(** [end_turn game msg] is the state of the game [game] after a player's turn
    has ended. The reason for the turn ending [reason_msg] is passed and 
    printed. Any input returns the user back to the main state of the game. *)
and end_game_transition game reason_msg= 
  print_endline reason_msg;
  print_endline "\nType anything to continue.\n";
  print_string ">";
  match read_line() with
  | _ -> end_game game 

(** [end_turn game msg] is the state of the game [game] after a player's turn
    has ended. The reason for the turn ending [reason_msg] is passed and 
    printed. Any input returns the user back to the main state of the game. *)
and end_turn game reason_msg= 
  print_endline reason_msg;
  print_endline "\nType anything to continue.\n";
  print_string ">";
  match read_line() with
  | _ -> repl game ""

(** [player_turn start game player_name end_time msg] is the active state of the
    game [game] for player [player_name] that expires after time [end_time].
    Handles any error message [error_msg] passed along. *)
and player_turn start game player_name end_time error_msg = 
  if not (List.mem player_name (get_player_names game))
  then repl game (player_name^" is not in the game!\n")
  else if get_turn_of_player game player_name = false 
  then repl game (player_name^" has already gone!\n");
  clear_screen ();
  print_board game.board;
  if start 
  then print_endline ("\n"^player_name^", you have 3 minutes. Go!")
  else print_endline ("\n"^player_name^", you have "^
                      string_of_time (end_time-.Unix.time ())
                      ^" remaining.");
  print_endline ("\nType \"word [your word]\" to submit a word"^
                 "\n\"words\" to see all the words you have submitted,"^
                 "\n\"hint\" to get a hint, or"^
                 "\n\"score\" to "^
                 "see your current score,"^
                 "\nwhich is updated at the end of each round.
                 \nType \"end\" to end your turn early.\n");
  if error_msg <> "" 
  then (print_endline error_msg; print_string "> ")
  else print_string "> ";
  match Command.active_parse(read_line()) with
  | ActiveQuit -> quit ()
  | Hint -> 
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time!"
    else
      (let hint = (game.possible_words |> Word.get_hint)^"\n" in
       player_turn false game player_name end_time hint)
  | End -> end_turn (expire_turn game player_name) "\nYou have ended your turn!"
  | Score -> 
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time!"
    else
      (let score = "Your score is "^ 
                   string_of_int (get_score_of_player game player_name)^"\n" in 
       player_turn false game player_name end_time score)
  | Words -> 
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time!"
    else
      (let words = ("Your words are: "^
                    (get_words_of_player game player_name
                     |> List.rev
                     |> string_of_list "")^"\n") in
       player_turn false game player_name end_time words)
  | Word w -> 
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time and this word will not count!"
    else if legal_word_in_board w game.board = false 
    then player_turn false game player_name end_time 
        "Not a word in the board.\n"
    else if not (is_english_word w) 
    then player_turn false game player_name end_time "Not an English word.\n"
    else if String.length w < 3 
    then player_turn false game player_name end_time 
        "Word must be at least three characters.\n"
    else player_turn false (add_word game player_name w) player_name end_time 
        ("Your word \"" ^ w ^ "\" has been accepted!\n")
  | exception Malformed -> 
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time!"
    else
      player_turn false game player_name end_time 
        "Not a legal word or command.\n"
  | exception Empty ->
    if Unix.time () > end_time 
    then end_turn (expire_turn game player_name) 
        "\nYou have used up all your time!"
    else
      player_turn false game player_name end_time 
        "No word or command has been input.\n"

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  clear_screen ();
  ANSITerminal.(print_string [red]
                  "
                  --------------------------------------
                  | Welcome to the Boggle Game engine! |
                  --------------------------------------\n");
  print_endline 
    ("\nType \"rules\" to read the game rules, or enter anything else to "^
     "start the game.");
  print_endline "Type \"quit\" at any time to exit the game.\n";
  print_string  "> ";
  match read_line () with
  | "rules" -> rules true ""
  | "quit" -> quit ()
  | _ -> start_game false ""

(** [rules repeat bad_command] is the rules state of the game. The prompt given
    to the user depends on whether the access to this state has been repeated
    [repeat] and any bad command [bad_command] is passed and handled. *)
and rules repeat bad_command = 
  clear_screen ();
  if repeat then 
    print_endline "\nRULES:\n
    The game begins with a 4x4 matrix of randomly generated letters (A-Z).
    The objective of the game is for each player to type as many words that can
    be created from the given board state as possible in a set amount of time 
    (3 minutes). Words are defined to be sequences of adjacent characters where 
    adjacent means letters that are vertically, diagonally, or horizontally 
    neighboring. 

    Once all players have gone, the score for the round is calculated as 
    follows. Any word that more than one player has is disregarded in
    calculating the score. Remaining words that are less than 3 characters 
    long recieve 0 points. Words of 3 or 4 characters receive 1 point each.
    Words of 5, 6, or 7 characters receive 2, 3, or 4 points respectively.
    Words longer than 7 characters receive 11 points. Any player who identifies
    more than 1/6th of all possible words will achieve silver rank, more than
    1/3rd of all possible words will achieve gold rank, more than 1/2 of all
    possible words will achieve platinum rank. Otherwise the player will be 
    given bronze rank for the round.

    Scores from one round may be aggregated by invoking a rematch at the end of
    the game. However, if a common word appears in the board of several boards,
    its points will count for each round (i.e. scores are calculated at the end
    of each round and added to the previous rounds' scores).
    \nNow, type \"start\" to start the game.\n"
  else
    print_endline ("\nRULES:\n 
    The game begins with a 4x4 matrix of randomly generated letters (A-Z).
    The objective of the game is for each player to type as many words that can
    be created from the given board state as possible in a set amount of time
    (3 minutes). Words are defined to be sequences of adjacent characters where
    adjacent means letters that are vertically, diagonally, or horizontally 
    neighboring.

    Once all players have gone, the score for the round is calculated as 
    follows. Any word that more than one player has is disregarded in
    calculating the score. Remaining words that are less than 3 characters 
    long recieve 0 points. Words of 3 or 4 characters receive 1 point each.
    Words of 5, 6, or 7 characters receive 2, 3, or 4 points respectively.
    Words longer than 7 characters receive 11 points. Any player who identifies
    more than 1/6th of all possible words will achieve silver rank, more than
    1/3rd of all possible words will achieve gold rank, more than 1/2 of all
    possible words will achieve platinum rank. Otherwise the player will be 
    given bronze rank for the round.

    Scores from one round may be aggregated by invoking a rematch at the end of
    the game. However, if a common word appears in the board of several boards,
    its points will count for each round (i.e. scores are calculated at the end
    of each round and added to the previous rounds' scores).
    \n\""^bad_command^"\" is not a recognizable command.
    \nType \"start\" to start the game or \"back\" to return to the start.\n");
  print_string  "> ";
  match read_line () with
  | "start" -> start_game false "";
  | "back" -> main ()
  | "quit" -> quit ()
  | bad_command -> rules false bad_command

(** [start_game repeat bad_command] is the state that initializes a game. 
    The prompt given to the user depends on whether the access to this state has
    been repeated [repeat] and any bad command [bad_command] is passed and 
    handled.*)
and start_game repeat bad_command= 
  clear_screen ();
  print_endline ("\nPlease enter the number of players (two or more) who "^
                 "would like to play this round.");
  print_endline ("\nIf you would like to go back to the main menu, "^
                 "type \"back\".\n");
  if repeat then 
    (print_endline ("\""^bad_command^
                    "\" is not a valid number of players.\n");
     print_string "> ";)
  else
    print_string "> ";
  match read_line () with
  | "quit" -> quit()
  | "back" -> main()
  | n -> 
    begin
      try let num_players = int_of_string n in 
        if num_players > 1 
        then setup_game num_players false []
        else start_game true n
      with exn -> 
        start_game true n
    end

(** [setup_game num_players repeat bad_list] is the state of the game that takes
    the player details for the number of players [num_players]. The prompt given
    to the user depends on whether the access to this state has been repeated
    [repeat]. Any bad list of player names [bad_list] is passed and handled. *)
and setup_game num_players repeat bad_list= 
  clear_screen ();
  print_endline ("\nPlease enter "^string_of_int num_players^
                 " unique player names separated by "^
                 "spaces (e.g. \"John Ron ...\"). ");
  print_endline ("\nIf you would like to go change the number of players, "^
                 "please type \"back\".\n");
  if repeat then 
    (print_endline ("Your input had "^(string_of_int (List.length bad_list))^
                    " names.
    \nYou must input exactly "^string_of_int num_players^
                    " unique names. Please ensure there are duplicate names.");
     print_string "\n> ";)
  else
    print_string "> ";
  let player_names = read_line () 
                     |> String.split_on_char ' ' 
                     |> List.filter (fun s -> s <> "") in
  match player_names with
  | ["back"] -> start_game false ""
  | ["quit"] -> quit ()
  | l -> 
    if (List.length l <> num_players) || l 
                                         |> List.sort_uniq compare 
                                         |> List.length <> num_players
    then (setup_game num_players true l)
    else (clear_screen ();
          print_endline "\n...Initializing the board and loading the game...";
          let board = init_board dice_sample1 in 
          let game = Game.init_game board l in 
          repl game "")

(** [rematch_game_helper old_game new_game player_names] matches [player_names]
    and transfers the score of player [h] if [player_names] from [old_game]
    into [new_game] if [player_names] is not empty. If [player_names] is empty
    then it returns [new_game] *)
and rematch_game_helper old_game new_game player_names = 
  match player_names with 
  | [] -> new_game 
  | h::t -> 
    let h_score = get_score_of_player old_game h in 
    let updated_game = set_score_of_player new_game h h_score in
    rematch_game_helper old_game updated_game t 

(** [rematch_game old_game] remakes a game with a new board but with 
    the same player names and their accompanying scores/ *)
and rematch_game old_game =
  clear_screen ();
  print_endline "\n...Initializing the board and loading the game...";
  let player_names = get_player_names old_game in 
  let board = init_board dice_sample1 in 
  let new_game = Game.init_game board player_names in 
  let updated_game = rematch_game_helper old_game new_game player_names in 
  repl updated_game ""

(* Execute the game engine. *)
let () = main ()


