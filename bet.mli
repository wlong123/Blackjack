(** This module includes the betting functions for Blackjack: split,
    stand, hit, double, and the optional insurance *)

(** The abstract type representing a bet *)
type t

exception Out_of_bounds

(** [init wallet min max] initializes the bet state with a chip count of 
    size [wallet], minimum bet of [min] and a maximum bet of [maximum]. *)
val  init : int -> int -> int -> t

(** [place_bet n b] is [b] with a bet of amount [n] and a chip count d
    ecreased by [n].
    Raises: [Out_of_bounds] if [n] > max bet amount of [b]. 
    [Out_of_bounds] if [n] < min bet of [b].
    [Out_of_bounds] if [n] > chip count of [b]. *)
val place_bet : int -> t -> t

(* [set_max_min max min b] is [b] with maximum and minimum 
   bets set to [max] and [min] respectively  *)
val set_max_min : int -> int -> t -> t

(* [reset_bet b] is [b] with the bet set to 0. *)
val reset_bet : t -> t

(** [set_temp_bet b] is [b] with a wallet equal to -[b]'s bet 
    This is used to take care of splitting. The negative bet accounts for the
    player placing an additional bet of the same amount on his second hand. *)
val set_temp_bet : t -> t

(** [get_wallet bet] is the number of available chips for betting in [bet]. *)
val get_wallet : t -> int

(** [get_bet bet] is the current bet of [bet]. *)
val get_bet : t -> int 

(** [get_min bet] is the minimum bet amount of [bet] *)
val get_min : t -> int

(** [get_max bet] is the maximum bet amount of [bet] *)
val get_max : t -> int

(** [double b] is [b] with the current bet of [b] doubled.
    Raises: [Out_of_bounds] if there isn't enough chips in [b] to double. *)
val double : t -> t 

(** [update_wallet n b] updates the player wallet by multiplying by a factor 
    that depends on the outcome of the game. For example if the player wins 
    then the multiplier is 2; if they tie, it's 1; if they get a blackjack 
    it's 2.5; and if they lose then it's 0 *)
val update_wallet : int -> t -> t

(** [add_wallets br bt] is a bet with the wallets of [br] and [bt] added.
    It is used when a user splits their hand and must collect the money of
    both of their plays at the end. *)
val add_wallets : t -> t -> t
