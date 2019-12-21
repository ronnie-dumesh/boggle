type object_phrase = string

type endgame_command =
  | Score
  | Allwords
  | Newgame
  | EndgameQuit

type passive_command = 
  | Start of string
  | PassiveQuit

type active_command = 
  | ActiveQuit
  | End
  | Words
  | Hint
  | Word of object_phrase
  | Score

exception Empty

exception Malformed

(** [break_string s] is a string list of the string [s] broken apart by 
    spaces. *)
let break_string s = 
  String.split_on_char ' ' s

(** [remove_spaces lst] Removes all empty strings in the string list [lst]. *)
let remove_spaces lst = 
  List.filter (fun s -> s <> "") lst

(** [get_head_or_error] Gets the head of a string list.
    Raises: Empty if the list is empty *)
let get_head_or_error = function
  | [] -> raise Empty
  | h::t -> h

(** [get_tail_or_error] Gets the tail of a string list.
    Raises: Empty if the list is empty *)
let get_tail_or_error = function
  | [] -> raise Empty
  | h::t -> t

let endgame_parse str =
  let hd = str
           |> break_string
           |> remove_spaces
           |> get_head_or_error in
  let tl = str 
           |> break_string
           |> remove_spaces
           |> get_tail_or_error in 
  if hd = "quit" && tl = [] then EndgameQuit
  else if hd = "score" && tl = [] then Score
  else if hd = "rematch" && tl = [] then Newgame
  else if hd = "words" && tl = [] then Allwords
  else raise Malformed 

let passive_parse str =
  let hd = str
           |> break_string
           |> remove_spaces
           |> get_head_or_error in
  let tl = str 
           |> break_string 
           |> remove_spaces 
           |> get_tail_or_error in
  if hd = "quit" && tl = [] 
  then PassiveQuit 
  else if hd = "start" && List.length tl = 1
  then Start (List.nth tl 0)
  else raise Malformed

let active_parse str =
  let hd = str 
           |> break_string
           |> remove_spaces 
           |> get_head_or_error in
  let tl = str 
           |> break_string 
           |> remove_spaces 
           |> get_tail_or_error in
  if hd = "quit" && tl = [] 
  then ActiveQuit
  else if hd = "end" && tl = [] 
  then End 
  else if hd = "words" && tl = []
  then Words
  else if hd = "hint" && tl = []
  then Hint
  else if hd = "score" && tl = []
  then Score
  else if hd = "word" && List.length tl = 1
  then Word (List.nth tl 0)
  else raise Malformed