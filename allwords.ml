open Board
open Word

type sblock = {
  letter : char;
  index : int;
  neighbors : int list;
  search_str : string;
  visited : int list;
  depth : int;
}

(**[get_sletter sblock] returns the letter of [sblock] *)
let get_sletter sblock = sblock.letter 

(**[get_sindex sblock] returns the index of [sblock] *)
let get_sindex sblock = sblock.index

(**[get_sneighbors sblock] returns the neighbors of [sblock] *)
let get_sneighbors sblock = sblock.neighbors

(**[get_sstr sblock] returns the search_string of [sblock] *)
let get_sstr sblock = sblock.search_str

(**[get_svisited sblock] returns the visited [sblocks]s of [sblock] *)
let get_svisited sblock = sblock.visited

(**[get_sdepth sblock] returns the depth of [sblock] *)
let get_sdepth sblock = sblock.depth

(**[get_sletter_from_sindex sboard idx] returns the letter of
   the [sblock] in [sboard] with index [idx]*)
let rec get_sletter_from_sindex sboard idx =
  match sboard with 
  | [] -> failwith "nth error"
  | h::t -> 
    if (get_sindex h) = idx
    then get_sletter h 
    else get_sletter_from_sindex t idx 

(**[make_new_sblock sboard sblock index] makes a new [sblock] 
   with the corresponding letter, [index], neighbors, the search_string
   of the previous [sblock], the visited of the index of the previous [sblock]
   appended to the visited of the previous [sblock], and the depth
   of the previous [sblock] - 1*)
let make_neighbor_sblock sboard sblock index = 
  let ltr = get_sletter_from_sindex sboard index in 
  {
    letter = ltr;
    index = index;
    neighbors = neighbor_indices index;
    search_str = (get_sstr sblock)^(String.make 1 ltr);
    visited = get_sindex sblock :: (get_svisited sblock);
    depth = get_sdepth sblock - 1;
  }

(**[iter_neigh s_board search_list sblock neigh_list] returns
   the neighbors as [sblock] as type [sblock] if it has not been
   visited by the [sblock].*)
let rec iter_neigh s_board search_list sblock = function  
  | [] -> search_list
  | h::t ->
    if List.mem h (get_svisited sblock)
    then iter_neigh s_board search_list sblock t
    else 
      let neighbor_sblock = make_neighbor_sblock s_board sblock h in 
      iter_neigh s_board (neighbor_sblock::search_list) sblock t

(** [make_neighbors s_board sblock acc sblock_list] returns
    the string represented by [sblock] of all blocks traversed up to that
    point prepended to [acc] in [iter_sblock_list s_board acc sblock_list]. 
    If the depth of [sblock] is greater than 1, then [sblock_list]
    has the neighbors of [sblock] prepended to it*)
let rec search_blocks s_board sblock acc sblock_list = 
  let retrn = 
    (get_sstr sblock) :: acc 
    |> iter_sblock_list s_board in 

  if get_sdepth sblock = 1 
  then retrn sblock_list
  else 
    let new_neighbors = get_sneighbors sblock 
                        |> iter_neigh s_board [] sblock  in 

    retrn (new_neighbors @ sblock_list)

(**[iter_sblock_list s_board acc bl]returns all the permutations of blocks up
   to the previously assigned depth in [acc] if [bl] is empty and otherwise
   returns [search_blocks s_board h acc t] with the head and tail of [bl]*)
and iter_sblock_list s_board acc = function 
  | [] -> acc
  | h::t -> search_blocks s_board h acc t

(**[make_search_blocks_from_board_blocks board depth block] makes
   search_blocks from [block] in [board]. A search_block has the
   same properties as a block, including a letter, index, and neighbors,
   but also adds a [depth], visited of [], and a search string of the
   block's letter *)
let make_search_blocks_from_board_blocks board depth block =
  let idx = get_index block in 
  let ltr = get_letter_from_index board (idx -1) in 
  {
    letter = ltr;
    index = idx; 
    neighbors = neighbor_indices idx;
    search_str = String.make 1 ltr;
    visited = []; 
    depth = depth; 
  }

let search_board board depth = 
  let s_board = 
    get_blocks board
    |> List.map (make_search_blocks_from_board_blocks board depth) in 

  iter_sblock_list s_board [] s_board

(* [check_vowels word] returns true if [word] contains a vowel,
one of 'a','e','i','o','u','y', and false otherwise *)
let check_vowels (word : string) : bool = 
  if String.contains word 'a' ||  String.contains word 'e' ||
  String.contains word 'i' || String.contains word 'o' ||
  String.contains word 'u'|| String.contains word 'y'
  then true
  else false 

let valid_words board depth = 
  search_board board depth 
  |> List.filter (fun s -> String.length s > 2 )
  |> List.filter is_common_word
  |> List.rev_map String.lowercase_ascii 
  |> List.filter check_vowels
  |> List.sort_uniq compare