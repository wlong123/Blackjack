(** [casino_dealer d] is the move a casino dealer would make if it were
    player [d] *)
val casino_dealer : Player.t -> Command.command

(** [good_move game p] is the move player [p] would make if they were to use
    perfect blackjack strategy in game [game]. Strategy taken from this site.
    https://www.blackjackarmy.com/basic-blackjack-strategy *)
val good_move : Blackjack.t -> Player.t -> Command.command


(** [wong_halves_count game p] is the card count when using the wong halves
    card counting strategy. Strategy taken from this website
    https://www.blackjackgala.com/counting-cards/wong-halves/ *)
val wong_halves_count : int -> Deck.rank list -> int

(** [cheater game p card] is the move player [p] would make if they could see 
    the top card of the deck *)
val cheater : Player.t -> Deck.card -> Command.command

(** [update_cc g] is a count of the cards that have been drawn in [g]. *)
val update_cc : Blackjack.t -> int

(** [card_counter game p] is the bet player [p] would place if they we counting
    cards in game [game]. *)
val card_counter : Blackjack.t -> Player.t -> int

(** [p_bust p] is the probability player [p] busts in [game] if [p]
    decides to hit given their current hand. *)
val p_bust : Player.t -> float

(** [p_advantage game] is a player's advantage over the dealer in [game]. *)
val p_advantage : Blackjack.t -> float