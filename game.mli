(** 
   Representation of static game data.

   This module represents the data stored in a game instance, including
   the board and players.
*)

(** The type of player. *)
type player = {
  name : string;
  words : string list;
  points : int;
  has_turn : bool;
}

(** The abstract type of values representing games. *)
type t = {
  players: player list;
  board:  Board.t;
  possible_words : string list;
}

(** [get_player_names game] is a string list of the names of the players in the
    game [game]. *)
val get_player_names: t -> string list

(** [get_players_left game] is a string list of the names of the players in the
    game [game] who have not yet used their turn. *)
val get_players_left: t -> string list

(** [get_words_of_player game player_name] is the list of words that player 
    [player_name] has input in the game [game]. Fails if there is not exactly 
    one player with the name [player_name] in the game [game]. *)
val get_words_of_player: t -> string -> string list

(** [get_score_of_player game player_name] is the int of points that player 
    [player_name] has gained in the game [game]. Fails if there is not exactly 
    one player with the name [player_name] in the game [game]. *)
val get_score_of_player: t -> string -> int

(** [get_turn_of_player game player_name] is the bool that indicates whether 
    that player [player_name] has finished its turn in the game [game]. 
    Fails if there is not exactly one player with the name [player_name] in the 
    game [game]. *)
val get_turn_of_player: t -> string -> bool

(** [no_turns_left players] is true iff all players [players] finished their 
    turns. *)
val no_turns_left: player list -> bool

(** [set_score_of_player game player_name score] changes the value of the 
    number of points the player with [player_name] has in [game]. Fails 
    if there is not exactly one player with the name [player_name] in the
    game [game]. A new game with the *)
val set_score_of_player: t -> string -> int -> t

(** [expire_turn game player_name] is the updated game.t after a player with 
    name [player_name] finishes its turn in the game [game]. *)
val expire_turn: t -> string -> t

(** [add_word game player_name word] adds [word] to [player_names]'s total  
    words in the [game]. *)
val add_word: t -> string -> string -> t

(** [init_game dice player_names] is an initialized game given a valid Boggle
    [board] setup and a list of [player_names]. *)
val init_game: Board.t -> string list -> t

(** [string_of_list acc lst] is a string representation of list [lst] building 
    from accumulator [acc]. *)
val string_of_list: string -> string list -> string

(** [print_words game players] is a print-statement of words that each player 
    [players] has found in the game [game]. *)
val print_words: t -> player list -> unit

(** [update_final_scores] updates the game with new player scores after 
    each round [game] is finished; each player [players] would have the final 
    score of the last round and an empty list of words. *)
val update_final_scores: t -> t

(** [print_words game players] is a print-statement of scores that each player 
    [players] has gained in the game [game] after score calculations. *)
val print_scores: t -> player list -> unit

(** [who_won players] is a player who gained the highest point in the game 
    [game]. *)
val who_won: player list -> player

(** [print_who_won game players] is a print-statement of a player who won the 
    game [game] among [players]. If all players [players] got the same points, 
    then it prints "Draw!"*)
val print_who_won: t -> player list -> unit

(** [print_rankings game] is a series of print_statement 
    that calculates the ranking ofthe player based on the number of points
    they got relative to the total number of points possible *)
val print_rankings: t -> unit 

(** [print_all_words game] prints all the possible words in the game 
    as part of the endgame*)
val print_all_words: t -> unit