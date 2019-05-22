open Deck
open Bet

type t = { 
  id: int;
  name: string;
  hand: Deck.card list;
  score: int; 
  is_dealer: bool;
  is_playing: bool; 
  is_cpu: bool;
  bet: Bet.t
}

let init id name hand dealer bet = { 
  id         = id; 
  name       = name;
  hand       = hand; 
  score      = Deck.score 0 hand; 
  is_dealer  = dealer;
  is_playing = true;
  is_cpu     = false;
  bet        = bet 
}

let init_cpu id name hand dealer bet = { 
  id = id; 
  name = name;
  hand = hand; 
  score = Deck.score 0 hand; 
  is_dealer = dealer;
  is_playing = true;
  is_cpu = true;
  bet = bet 
}

let set_bet p b = 
  { p with bet = b}

let is_cpu p = p.is_cpu

let number_of_cards p = 
  List.length p.hand

let is_dealer p = 
  p.is_dealer

let get_score p =
  p.score

let get_id p = 
  p.id

let get_hand p = p.hand

let is_playing p = p.is_playing

let get_name p = p.name

let get_bet p = p.bet

let bust p = 
  { p with is_playing = false }

let place_bet n p =
  { p with bet = place_bet n p.bet }

let payout n p = 
  { p with bet = update_wallet n p.bet }

let merge_wallet pr pt =
  { pr with bet = add_wallets pr.bet pt.bet }

let reset_player p = 
  { p with score = 0; 
           hand  = [] ; 
           bet   = Bet.reset_bet p.bet ; 
           is_playing = true }

let compare21 p = 
  if p.score < 21  then -1
  else if p.score > 21 then 1
  else 0

let update_hand card p = 
  let new_hand = card::p.hand in
  let new_score = Deck.score p.score [card] in 
  if new_score > 21 then 
    let try_downgrade = Deck.downgrade_ace new_hand in
    let downgrade_score = Deck.score 0 try_downgrade in
    { p with hand = try_downgrade; score = downgrade_score }
  else { p with hand = new_hand; score = new_score }