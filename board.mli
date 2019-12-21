(** The type of a block on the Boggle board. *)
type block = {
  letter : char;
  index : int;
  neighbors : int list;
}

(** The abstract type representing the Boggle board. *)
type t = {
  blocks : block list
}

(** [get_index block] returns the index of [block]*)
val get_index : block -> int 

(** [dice_sample1] is the standard sample of Boggle dice. *)
val dice_sample1 : string list

(** [neighbor_indices idx] is the list of integer indices that neighbor index
    [idx]. *)
val neighbor_indices : int -> int list

(** [init_board d] is a new randomized instance of the Boggle board given a 
    set of dice [d]. *)
val init_board : string list -> t

(** [get blocks board] is a list of [block]s of the [board]*)
val get_blocks: t -> block list

(**[idx_to_block idx b] returns the [i]th block of board [b]*)
val idx_to_block: t -> int -> block

(**[letter_to_block letter b] returns all blocks with the letter [l] in [b]*)
val letter_to_block: t -> char -> block list

(**[board_to_list b] returns all the blocks within the board [b] *)
val board_to_letter_list: t -> char list

(** [get_letter_from_index board idx] is a char of the corresponding [idx] in 
    the [board]*)
val get_letter_from_index : t -> int -> char 

(** [get_letter d block] is the letter of block [block] in [d] *)
val get_letter: t -> block -> char 

(**[is_word] returns whether a word does not contain any spaces or 
   special characters *)
val is_word: string -> bool

(**[legal_word_in_board word] returns whether a word is capable of being
   accessed in the board *)
val legal_word_in_board: string -> t -> bool

