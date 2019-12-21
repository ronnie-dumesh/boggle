(**[search_board board depth] returns all the possible permutations of letters
   in the board from length [1..depth] where 1 < depth < 15. search_board
   may return duplicates*)
val search_board : Board.t -> int -> string list

(**[all_valid_words] returns most possible words one can make up in the 
   board so that the words would be from length [3..depth] where
   3 < depth < 15. All boggle words must be at least of length 3.
   There are no duplicates. *)
val valid_words : Board.t -> int -> string list