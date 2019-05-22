exception Out_of_bounds

type t = { player_bet : int;
           wallet     : int;
           min_bet    : int; 
           max_bet    : int }

let init wallet min max = 
  { player_bet = 0;
    wallet     = wallet;
    min_bet    = min;
    max_bet    = max }

let place_bet n b =
  if n < b.min_bet || n > b.max_bet || n > b.wallet 
  then raise Out_of_bounds 
  else { b with player_bet = n; wallet = b.wallet - n } 

let set_max_min max min b =
  { b with max_bet = max; min_bet = min }

let reset_bet b = 
  { b with player_bet = 0 }

let set_temp_bet bet = 
  { bet with wallet = -bet.player_bet }

let get_wallet bet = bet.wallet

let get_bet bet = bet.player_bet

let get_min bet = bet.min_bet

let get_max bet = bet.max_bet

let double b = 
  if b.player_bet > b.wallet then raise Out_of_bounds else
    { b with player_bet = 2 * b.player_bet; wallet = b.wallet - b.player_bet }

let update_wallet n b = 
  { b with wallet = b.wallet + (n * b.player_bet) }

let add_wallets br bt =
  { br with wallet = br.wallet + bt.wallet }