
(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. These commands are relevant
    to the end game state of the game i.e. when all players have completed
    their turn. *)
type endgame_command =
  | Score
  | Allwords
  | Newgame
  | EndgameQuit

(** The type [passive_command] represents a player command that is decomposed
    into a verb and possibly an object phrase. These commands are relevant
    to the passive state of the game i.e. the main state of an initialized
    game. *)
type passive_command = 
  | Start of string
  | PassiveQuit

(** The type [active_command] represents a player command that is decomposed
    into a verb and possibly an object phrase. These commands are relevant
    to the active state of the game i.e. a player's turn. *)
type active_command = 
  | ActiveQuit
  | End
  | Words
  | Hint
  | Word of object_phrase
  | Score

exception Empty

exception Malformed

(** [endgame_parse str] parses a player's input into a [command] 

    "quit", "words", "new", and "score" must all not be followed
    by any other input

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is {i malformed}
    if the input is neither quit, words, new, nor score*)
val endgame_parse : string -> endgame_command

(** [active_parse str] parses a player's input into a [command] such that 
    the first word is a command and all following words become 
    part of the object phrase

    "quit", "words", "end", and "score" must all not have object phrases

    "word" requires an object phrase and submits the object phrase
    as a possible word. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val passive_parse : string -> passive_command

(** [active_parse str] parses a player's input into a [command] such that 
    the first word is a command and all following words become 
    part of the object phrase

    "quit", "start" must all not have object phrases

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val active_parse : string -> active_command