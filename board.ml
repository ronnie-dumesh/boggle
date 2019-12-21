type block = {
  letter : char;
  index : int;
  neighbors : int list;
}

type t = {
  blocks : block list;
}

let dice_sample1 = ["AAEEGN"; "ABBJOO"; "ACHOPS"; "AFFKPS";
                    "AOOTTW"; "CIMOTU"; "DEILRX"; "DELRVY";
                    "DISTTY"; "EEGHNW"; "EEINSU"; "EHRTVW";
                    "EIOSST"; "ELRTTY"; "HIMNQU"; "HLNNRZ"]

(** [i -- j] is the list of integers from [i] to [j], inclusive.
    Tail recursive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(** [shuffle l] is a randomly sorted permutation of the list [l]. *)
let rec shuffle l = 
  ignore (Random.init (int_of_float (1000.*.Unix.time ())));
  match l with
  | [] -> []
  | [single] -> [single]
  | list -> 
    let (before, after) = List.partition (fun elt -> Random.bool ()) list in 
    List.rev_append (shuffle before) (shuffle after)

(** [random_char str len] is a random character from the first [len] characters
    of the string [str]. 
    Requires: [str] is at least [len] characters long. *)
let random_char str len = 
  let i  = Random.int len in
  str.[i]

(** [get_letters acc d] is a set of random letters from a set of dice [d]. *)
let rec get_letters acc = function
  | [] -> acc
  | h::t -> 
    let c = random_char h 6 in
    get_letters (c::acc) t

(** [letters d] is a list of position, character tuples randomly generated from
    the set of dice [d]. *)
let letters d = 
  d 
  |> shuffle 
  |> get_letters []
  |> List.combine (1--16)

(** [get_index block] is an index that corresponds to the [block]*)
let get_index block = block.index 

(** [index_neighbors idx1 idx2] is true iff index [idx1] and index [idx2] are
    neighbors on the 4x4 Boggle grid. *)
let index_neighbors idx1 idx2 = 
  let careful_mod a n =
    let v = (a mod n) in
    if v = 0 then n else v in
  let r1 = int_of_float (((float_of_int idx1) -. 1.) /. 4.) in
  let r2 = int_of_float (((float_of_int idx2) -. 1.) /. 4.) in
  abs (careful_mod idx1 4 - careful_mod idx2 4) <= 1 && abs (r1 - r2) <= 1

let neighbor_indices idx = 
  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16] 
  |> List.filter (index_neighbors idx)
  |> List.filter (fun e -> e <> idx)

let init_board dice =
  let ltrs = letters dice in
  let rec ltrs_to_blocks ltrs acc =
    match ltrs with 
    | [] -> acc
    | (idx, char)::t -> 
      ltrs_to_blocks t ({letter = char;
                         index = idx;
                         neighbors = neighbor_indices idx}::acc) in
  {blocks = (ltrs_to_blocks ltrs []) @ []}

let get_blocks board = board.blocks 

let idx_to_block board idx=
  List.find (fun x-> x.index=idx) (get_blocks board)

let letter_to_block board letter =
  List.filter (fun x-> x.letter=letter) (get_blocks board)

let board_to_letter_list board=
  let block_list= get_blocks board in
  let rec help lst acc=
    match lst with
    |[]->acc
    |h::t-> help t (h.letter::acc) in
  help block_list []

let get_letter_from_index board idx =
  let lst = board_to_letter_list board in 
  List.nth lst (idx)

let get_letter board block = 
  get_index block |> 
  get_letter_from_index board

(** [neighboring_characters idx board] is a list of neighboring characters to a block
    that has [idx] as its index in the [board]*)
let neighboring_characters idx board =
  let block = idx_to_block board idx in
  let neighbors block board =
    let rec help lst board acc =
      match lst with
      |[] -> acc
      |h::t -> let a = List.find (fun x -> x.index = h) board in
        help t board (a.letter::acc) in
    help block.neighbors board [] in
  neighbors block board.blocks

(** [list_char str acc] is a char list of characters in [str]*)
let list_char str=
  let rec help str acc=
    match str with
    |"" -> acc
    |a -> help (String.sub a 1 (String.length a-1))(String.get a 0::acc) in
  help str []|>List.rev

(** [find_letter char board] is a list of integers which are indices of the charcater [char] in the [board]. *)
let find_letter char (board:t)=
  let rec help char blocks acc=
    match blocks with
    |[]-> acc
    |h::t-> if char= h.letter then help char t (h.index::acc)
      else help char t acc in
  (* if help char (Board.blocks board) [] = [] then raise Not_found else *)
  (help char (get_blocks board) [])|>List.rev

let is_word word =
  let word' = String.lowercase_ascii word in
  let regex = (Str.regexp "^[a-z]+$") in
  Str.string_match regex word' 0

let legal_word_in_board word (board:t)=
  let upper_word = String.uppercase_ascii word in
  let lst_char= (list_char upper_word) in
  let rec help lst board =
    match lst with
    |[]-> false
    |h::t-> 
      if t = []
      then true 
      else let pos_in_board= List.rev(find_letter h board) in
        let rec help2 pos_list board =
          match pos_list with
          |[]-> false
          |a::b-> let next = (List.hd t) in
            if List.exists (fun x-> x = next)
                (List.rev(neighboring_characters a board))
            then help t board
            else help2 b board in
        help2 pos_in_board board in
  help lst_char board
