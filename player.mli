open Deck
open Bet

(** Representation of a player of blackjack. It holds a player's score, hand,
    and whether they are the dealer or not.

    This module handles updating all the information of the player as the game 
    progresses ans well as querying the data.

*)

(** The abstract type of values representing players. *)
type t (* record or tuple: player's hand, score, is_dealer *)

(** [init id name hand dealer bet] is an intance of a player with 
    id [id], name [name], hand [hand], and bet [bet]. If [dealer] is  
    true, then this player is set to the dealer. *)
val init : int -> string -> Deck.card list -> bool -> Bet.t -> t

(** [init_cpu id name hand dealer bet] is the same as [init] except it
    creates a computer player. *)
val init_cpu : int -> string -> Deck.card list -> bool -> Bet.t -> t

(** [is_cpu p] is whether [p] is a computer player. *)
val is_cpu : t -> bool

(** [update_hand card p] is player [p] with [card] added to their hand and 
    score updated to include that card. *)
val update_hand : Deck.card -> t -> t 

(** [reset_player p] is the player [p] with an empty hand, a score of 0, and
    an empty bet. *)
val reset_player : t -> t

(** [set_bet p b] is player [p] with bet set to [b]. *)
val set_bet : t -> Bet.t -> t

(** [compare21 p] is -1 if player [p]'s score is below 21, 0 if it equals 21,
    and 1 if it is over 21. *)
val compare21 : t -> int

(** [number_of_cards p] is the number of cards in player [p]'s hand. *)
val number_of_cards : t -> int

(** [is_dealer p] is true is [p] is the dealer of the game, false otherwise. *)
val is_dealer : t -> bool 

(** [get_score p] is the score of player [p]. *)
val get_score : t -> int

(** [get_id p] is the id of [p]. *)
val get_id : t -> int

(** [get_hand p] is the cards in the hand of player [p] *)
val get_hand : t -> Deck.card list

(** [get_name p] is the name of [p]. *)
val get_name : t -> string

(** [is_playing p] is true if the player has not gone bust, false otherwise. *)
val is_playing : t -> bool

(** [get_bet p] is the bet of [p]. *)
val get_bet : t -> Bet.t

(** [bust p] is a new player with is_playing set to false. *)
val bust : t -> t

(** [place_bet n p] is [p] after placing a bet of amount [n]. *)
val place_bet : int -> t -> t

(** [payout n p] is [p] after getting their payout at the end of a round. *)
val payout : int -> t -> t

(** [merge_wallet pr pt] is [pr] with their money and the money that [pt]
    has won. *)
val merge_wallet : t -> t -> t