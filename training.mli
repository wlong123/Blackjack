(* Description of module. *)


(** [get_char ()] is the first character inputted by the user. No enter is
    needed to retrieve character. 
    We obtained the algorithm for this from this site:
    https://stackoverflow.com/questions/13410159/how-to-read-a-character-in-ocaml-without-a-return-key *)
val get_char : unit -> char

(** [print_cards lst] prints [lst] using ascii art. *)
val  print_cards : Deck.card list -> unit

(** [init unit] is an instance of a training game mode. *)
val init : unit -> unit

