open Player
open Command
open Deck
open Blackjack
open Bet

let casino_dealer d =
  match Player.get_score d with
  | n when n <= 16 -> Hit
  | _ -> Stay

(** [hard_strategy d p] is the perfect strategy for player [p] given the
    top card of dealer [d] when player [p] has more than 2 cards. *)
let hard_strategy d p = 
  let player_score = Player.get_score p in
  if player_score >= 17 then Stay else
    match player_score,(Deck.score 0 [d]) with
    | p,d when p >= 13 && d <= 6 -> Stay
    | p,d when p = 12 && d >= 4 && d <= 6 -> Stay
    | p,d when p = 11 && d <= 10 -> Double
    | p,d when p = 10 && d <= 9 -> Double
    | p,d when p = 9 && d >= 3 && d <= 5 -> Double
    | _ -> Hit

(** [is_pair_or_has_ace p] is whether [p] has an ace or contains a pair. *)
let is_pair_or_has_ace p = 
  let player_hand = List.map Deck.number_of_card (Player.get_hand p) in
  match player_hand with
  | [Ace _;_]
  | [_;Ace _] -> true
  | [n;m] when n = m -> true
  | _ -> false

(** [soft_strategy d p] is the perfect strategy for player [p] given the
    top card of dealer [d] when player [p] has exactly 2 cards. *)
let soft_strategy d p = 
  let player_hand = List.map Deck.number_of_card (Player.get_hand p) in
  match player_hand,(Deck.rank d) with
  | [Ace _;Ace _],_ 
  | [Number 8;Number 8],_ -> Split
  | [Number 9;Number 9], Number n when n <> 7 && n <> 10 -> Split
  | [Number 7;Number 7], Number n when n <= 7 -> Split
  | [Number 6;Number 6], Number n when n <= 6 -> Split
  | [Number 3;Number 3], Number n when n <= 7 -> Split
  | [Number 2;Number 2], Number n when n <= 7 -> Split
  | [Number 10;Number 10],_ -> Stay
  | [Number 9;Number 9],Number n when n = 7 || n = 10 -> Stay
  | [Number 9;Number 9],Ace _ -> Stay
  | [Ace _;Number n],_ when n >= 8 && n <= 10 -> Stay
  | [Number n;Ace _],_ when n >= 8 && n <= 10 -> Stay
  | [Number 7;Ace _],Number n when n = 2 || n = 7 || n = 8 -> Stay
  | [Ace _;Number 7],Number n when n = 2 || n = 7 || n = 8 -> Stay
  | [Number 5;Number 5],Number n when n <= 9 -> Double
  | [Ace _;Number 7],Number n when n >= 3 && n <= 6 -> Double
  | [Number 7;Ace _],Number n when n >= 3 && n <= 6 -> Double
  | [Number 6;Ace _],Number n when n >= 3 && n <= 6 -> Double
  | [Ace _;Number 6],Number n when n >= 3 && n <= 6 -> Double
  | [Ace _;Number 5],Number n when n >= 4 && n <= 6 -> Double
  | [Number 5;Ace _],Number n when n >= 4 && n <= 6 -> Double
  | [Number 4;Ace _],Number n when n >= 4 && n <= 6 -> Double
  | [Ace _;Number 4],Number n when n >= 4 && n <= 6 -> Double
  | [Ace _;Number 3],Number n when n >= 5 && n <= 6 -> Double
  | [Number 3;Ace _],Number n when n >= 5 && n <= 6 -> Double
  | [Number 2;Ace _],Number n when n >= 5 && n <= 6 -> Double
  | [Ace _;Number 2],Number n when n >= 5 && n <= 6 -> Double
  | _ -> Hit

let good_move game p = 
  match Bet.double (Player.get_bet p) with
  | exception Out_of_bounds -> casino_dealer p
  | _ ->
    let dealer_card = Blackjack.top_dealer_card game in
    if Player.number_of_cards p > 2 || (not (is_pair_or_has_ace p)) 
    then hard_strategy dealer_card p else soft_strategy dealer_card p

let rec wong_halves_count acc = function
  | [] -> acc
  | (Ace _)::t
  | (Number 10)::t -> wong_halves_count (acc-2) t
  | (Number 9)::t  -> wong_halves_count (acc-1) t
  | (Number 8)::t  -> wong_halves_count (acc) t
  | (Number 7)::t 
  | (Number 2)::t  -> wong_halves_count (acc+1) t
  | (Number 3)::t 
  | (Number 4)::t 
  | (Number 6)::t  -> wong_halves_count (acc+2) t
  | (Number 5)::t  -> wong_halves_count (acc+3) t
  | _ -> failwith "invalid card"


let cheater p card = 
  match Player.compare21 (Player.update_hand card p) with
  | 1 -> Stay
  | _ -> Hit


let update_cc g =
  let cc = (Deck.filter [] (
      fun c -> not (Deck.mem c (get_deck g))) (Deck.make ())) 
           |> List.map Deck.number_of_card
           |> wong_halves_count 0 in 
  cc - (wong_halves_count 0 
          [(get_player 0 g 
            |> get_hand 
            |> ( fun x -> List.nth x 1)
            |> Deck.number_of_card)])

let card_counter g p = 
  let base = p |> Player.get_bet |> Bet.get_min in 
  let run_count = update_cc g in 
  match run_count with 
  | n when n <= 1 -> base 
  | n when n = 2 || n = 3 -> (2*base) 
  | n when n = 4 || n = 5 -> (3*base) 
  | n when n = 6 || n = 7 -> (4*base) 
  | n when n >= 8 -> (5*base) 
  | n -> failwith "Error in Card Counter"

let p_bust p =
  match Player.get_score p with
  | n when n >= 21 -> 100.0
  | 20 -> 92.0
  | 19 -> 85.0
  | 18 -> 77.0
  | 17 -> 69.0
  | 16 -> 62.0
  | 15 -> 58.0
  | 14 -> 56.0
  | 13 -> 39.0
  | 12 -> 31.0
  |  _ -> 0.0

(** [dealer_card_advantage game] is a players advantage given the dealer's
    top card in [game]. *)
let dealer_card_advantage game =
  match Deck.rank (Blackjack.top_dealer_card game) with
  | Ace _ -> -16.0
  | King
  | Queen
  | Jack
  | Number 10 -> -16.9
  | Number 9 -> -4.3
  | Number 8 -> 5.4
  | Number 7 -> 14.3
  | Number 6 -> 23.9
  | Number 5 -> 23.2
  | Number 4 -> 18.0
  | Number 3 -> 13.4
  | Number 2 -> 9.8
  | _ -> failwith "invalid card"

(** [card_advantage acc cards] is the total player advantage of [cards] having
    been played in a game. *)
let rec card_advantage acc = function
  | [] -> acc
  | (Ace _)::t     -> card_advantage (acc -. 0.59) t
  | (Number 10)::t -> card_advantage (acc -. 0.51) t
  | (Number 9)::t  -> card_advantage (acc -. 0.15) t
  | (Number 8)::t  -> card_advantage (acc +. 0.01) t
  | (Number 7)::t  -> card_advantage (acc +. 0.30) t
  | (Number 6)::t  -> card_advantage (acc +. 0.45) t
  | (Number 5)::t  -> card_advantage (acc +. 0.67) t
  | (Number 4)::t  -> card_advantage (acc +. 0.52) t
  | (Number 3)::t  -> card_advantage (acc +. 0.43) t
  | (Number 2)::t  -> card_advantage (acc +. 0.40) t
  | _ -> failwith "invalid card"


(** [played_card_advantage game] is a players advantage given the cards that
    have already been played in [game]. *)
let played_card_advantage game =
  (Deck.filter [] (fun c -> not (Deck.mem c (get_deck game))) (Deck.make ()) ) 
  |> List.map Deck.number_of_card
  |> card_advantage 0.0 

let p_advantage game = 
  played_card_advantage game +. dealer_card_advantage game

