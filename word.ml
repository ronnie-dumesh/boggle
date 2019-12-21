open Yojson.Basic.Util

(** [hash_helper hash word] helps enable adding word [word] to the hash table
    [hash]. *)
let hash_helper hash word  = 
  Hashtbl.add hash word true; hash

(** [from_json j size] returns a hashmap with key, value pairs
    of words in [j]. [size] is the expected size of the hashmap and only
    needs to be an estimate, it will not affect the return of the function.*)
let from_json json size =
  let hash = Hashtbl.create size in 
  let words_list = json |> to_assoc |> List.rev_map fst in 
  List.fold_left hash_helper hash words_list

(** [make_dictionary file_name size] returns a dictionary that contains 
    every single word in the English language. [size] is the expected size of 
    the dictionary and only needs to be an estimate, it will not affect the
    return of the function. 

    Requires: [file_name] is a json file that contains all the words
    in the English language*) 
let make_dictionary file_name size = 
  from_json (Yojson.Basic.from_file file_name) size 

let large_english_dictionary = make_dictionary "words_dictionary.json" 500000

let small_english_dictionary = make_dictionary "clean_dict.json" 100000

let is_common_word w = 
  w 
  |> String.lowercase_ascii
  |> Hashtbl.mem small_english_dictionary

let is_english_word w = 
  w
  |> String.lowercase_ascii
  |> Hashtbl.mem large_english_dictionary 

let turn_to_hint word =
  let length = String.length word in
  let shown_chars = 
    if length < 6 then 2 
    else if length < 8 then 3
    else 4 in 
  (String.sub word 0 shown_chars) ^ (String.make (length - shown_chars) '*')

let get_hint all_words = 
  ignore (Random.init (int_of_float (1000.*.Unix.time ())));
  List.length all_words
  |> Random.int 
  |> List.nth all_words
  |> turn_to_hint

(** [get_score word other_words dictionary] returns the score 
    of a word in a game of Boggle. The score of [word] is  0 if [word] is not a 
    word in the English language as determined by whether it appears in 
    [large_english_dictionary] or if [word] appears in [other_words]. 
    Otherwise it returns 0 if [word] is of length 0, 1, or 2; 1 if of length 
    3 or 4; 2 if of length 5; 3 if of length 6; 4 if of length 7; and 11 if
    of length 8 of greater *)
let get_score word other_words = 
  if is_english_word word = false 
  then 0 
  else if List.mem word other_words
  then 0
  else 
    let length = String.length(word) in 
    match length with 
    | 0
    | 1
    | 2 -> 0
    | 3
    | 4 -> 1
    | 5 -> 2
    | 6 -> 3
    | 7 -> 4
    | _ -> 11

(** [calculate_score player_words other_words dictionary score] returns
    the [score] of [player_words] as determined by [get_score h other_words
    dictionary] *)
let rec calculate_score player_words other_words score =
  match player_words with 
  | [] -> score 
  | h::t -> calculate_score t other_words (score + get_score h other_words) 

let player_score player_words other_words =
  calculate_score player_words other_words 0

