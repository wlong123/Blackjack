open Deck 
open Player
open Yojson.Basic.Util

(** 
   Representation of blackjack game data.

   This module represents the data required to carry out a game of blackjack, 
   including player data, dealer, and deck. It handles initialization of all 
   the data as well as querying the data.
*)

(** The abstract type of values representing a Blackjack game. *)
type t

exception Not_found

(** [init n p_lst cpu_lst] is an instance of the game, blackjack, with [n] 
    players, and a full deck.  
    Requires: [n] is greater than or equal to 0. *)
val init : int -> string list -> string list -> t

(** [reset g] is a sate of [g] with all values initialized to start 
    a new round. *)
val reset : t -> t

(** [get_player i game] is a player in [game] with id equal to [i]. 
    Raises Not_found if no player exists in [game] with id [i]*)
val get_player : int -> t -> Player.t

(** [get_score p game] is the score of player [p] in [game]. 
    Requires: [p] is a player of this Blackjack game. *)
val get_score : int -> t -> int

(** [get_deck game] is the deck of [game]. *)
val get_deck : t -> Deck.t

(** [players game] is a list of all the players in game [game] *)
val players : t -> Player.t list

(** [is_player p game] is true if [p] is a player of [game],
    false otherwise. *)
val is_player : Player.t -> t -> bool

(** [get_dealer_score g] is the score of the dealer in [g]. *)
val get_dealer_score : t -> int

(** [top_dealer_card g] is the dealer's card that is initially right side up.*)
val top_dealer_card : t -> Deck.card

(** [hidden_card g] is the dealer's hidden card *)
val hidden_card : t -> Deck.card 

(** [hit game player] is the result of player [player] hitting in game [game].
    If player is not in the game, it remains as [game]. *)
val hit : t -> Player.t -> t

(** [rank_players game] is the list of players in game [game] sorted 
    by best score. The highest score would be 21, and the lowest would be 0. 
    If the player has gone bust then they will not be ranked. *)
val rank_players : t -> (Player.t list * Player.t list * Player.t list)

(** [save_state g s] saves game [g] in a file named [s].json. *)
val save_state : t -> string -> unit

(** [load_state json] loads a game from a json file [json]. *)
val load_state : Yojson.Basic.json -> t

(** [bet game player n] is [game] with the bet of [player] updated to [n] *)
val bet : t -> Player.t -> int -> t

(** [remove_players game] is [game] with all the players with no money left in
    thier wallet removed. *)
val remove_players : t -> t

(** [remove_player game p] is [game] with [p] removed. *)
val remove_player : t -> Player.t -> t

(** [need_to_switch game] is whether a player in [game] has fewer chips
    than the minimum chip count at the table they are currently at. *)
val need_to_switch : t -> bool

(** [remove_under_min game] is [game] with the players under the table
    minimum removed. *)
val remove_under_min : t -> t

(** [update_wallets g] is a [g] with all the player wallets updated to account 
    for how much money they won during the last round. *)
val update_wallets : t -> t

(** [split game p card1 card2] is [game] with the player list updated to include
    player p with hand = [card1] player p added again with hand = [card2] *)
val split : t -> Player.t -> Deck.card -> Deck.card -> t

(** [clean_after_split game] is [game] with the temporary players removed.
    Temporary players are used when a player makes a split. *)
val clean_after_split : t -> t

(** [double game pplayer_id] is [game] with a player, with player id 
    [player_id], added back to the list after doubling bet. *)
val double : t -> int -> t

(** [change_table min max g] is a game with the same players as [g], but 
    a different table, with [min] and [max] values for the min and max of 
    betting and a new set of cards. *)
val change_table : int -> int -> t -> t