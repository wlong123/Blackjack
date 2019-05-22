(** Representation of a standard 52-card Deck with some or none of the cards
    removed. 

    This module handles shuffling, querying, and altering the data.

*)

(** The abstract type of values representing decks. *)
type t

(** the type of the suit of a card *)
type suit = Club | Spade | Heart | Diamond

(** the type of the rank of a card *)
type rank = Number of int | Jack | Queen | King | Ace of int

(** The type of values representing a card. *)
type card = suit * rank

(** Raised when the deck is empty. *)
exception Empty

(** [make ()] is a full deck of 52 cards. *)
val make : unit -> t 

(** [shuffle deck] is [deck] with the cards in a randomized order  *)
val shuffle : t -> t

(** [size deck] is the number of cards in deck [deck]  *)
val size : t -> int

(** [draw deck] is the top card in [deck] and the [deck] with the top card
    removed.
    Raises: [Empty] if [deck] has no cards in it *)
val draw : t -> (card * t)

(** [suit c] is the suit of card [c] *)
val suit : card -> suit

(** [rank c] is the rank of card [c] *)
val rank : card -> rank

(** [score acc hand] is the score of the cards in [hand] with acc added. 
    The score of an empty [hand] is acc. *)
val score : int -> card list -> int

(** [downgrade_ace c] is [c] unless [c] contains an [Ace 11] in which case
    that card in [c] becomes an [Ace 1] *)
val downgrade_ace : card list -> card list

(** [card c] is a string representation of card [c] *)
val card : card -> string

(** [index_of_card c] is a touple of the index of card [c]'s
    rank in range [1:13] and suit in range [0:3]. Where 0, 1, 2, 3, correspond
    to Spade, Heart, Diamond, and Club respectively. *)
val index_of_card : card -> (int * int)

(** [card_to_number c] is [c] as a number representation.
    Example: [card_to_number King] is [Number 10] 
    [card_to_number Number 5] is [Number 5]*)
val number_of_card : card -> rank 

(** [mem c d] is true if the card [c] is in the deck [d] *)
val mem : card -> t -> bool

(** [filter acc f lst] is all the elements of the list l that satisfy the 
    predicate f with acc appended at the end. 
    The order of the elements in the input list is preserved. *)
val filter : card list -> (card -> bool) -> t -> card list

(*  [map f lst] applies function f all the elements of [lst] and builds the list 
    [f a1; ...; f an] where each an is an element of [lst]. *)
val map : (card -> 'a) -> t -> 'a list
