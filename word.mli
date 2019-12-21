(** [large_english_dictionary] returns a hashmap of every word in the English
    dictionary. *) 
val large_english_dictionary : (string, bool) Hashtbl.t 

(** [is_english_word w] returns whether w is a valid English word

    whether a word is determined to be valid depends on whether it is present in 
    the association list created by https://github.com/dwyl/english-words 
    This implementation parses a JSON file and creates a map *) 
val is_english_word : string -> bool

(** [small_english_dictionary] returns a hashmap of every word in an 
    abridged English dictionary *)
val small_english_dictionary : (string, bool) Hashtbl.t

(** [is_common_word w] returns whether w is a common English word 
    
    whether a word is determined to be common is determined on how it
    passed a spell-checking software. It is possible for a word to be an
    English word and not recognized as common.*)
val is_common_word : string -> bool

(** [turn_to_hint word] returns a string that represents the word with
    some of its letters hidden as to represnt a hint. A word of length
    3, 4, or 5 has its first two-characters non-obscured, 6 or 7 has its
    first three non-obscured, and anything greater has the first four
    non-obscured. The remaining characters in the word are replaced
    by asteriks. 

    Precondition: word is at least of length 3

    turn_to_hint "hello" yields "he***"
    turn_to_hint "seventy" yields "sev****"
    turn_to_hint "cabbages" yeilds "cab*****" *)
val turn_to_hint : string -> string


(** [get_hint all_words] returns a random word possible in the board
    with the last several characters obscured by asterisks. 

    Precondition: all_words is the list of all possible words in 
    a boggle board*)
val get_hint : string list -> string 

(** [player_score player_words other_words dictionary] returns
    the score of the player as according to the rules of Boggle. It checks
    whether each word in [player_score] is present in [dictionary] and 
    not present in [other_words], in which case it returns 
    a score proportional to the length of the given word as according
    to the rules of boggle*)
val player_score : string list -> string list -> int 