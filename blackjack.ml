open Bet 
open Deck 
open Player
open Yojson.Basic.Util
open Formatc

type t = { players: Player.t list;
           deck: Deck.t;}

exception Not_found

let rec get_player i game = 
  match game.players with 
  |[]-> raise Not_found
  |h::t -> if get_id h = i then h else 
      let new_game = { game with players = t } in 
      get_player i new_game

let get_score p game = get_player p game |> Player.get_score 

let get_deck game = game.deck

let players game = game.players

let is_player p game = 
  List.exists (fun x -> x = p) game.players

let get_dealer_score g = Player.get_score (get_player 0 g)

let top_dealer_card g = 
  g |> get_player 0 |> get_hand |> List.hd

let hidden_card g =  g |> get_player 0 |> get_hand |> List.tl|>List.hd

(* [update_players card player] 
   if drawing [card] caused [player] to bust
   then it is a list of players with [player] removed
   else it is a list of players with
   [player]'s hand updated to contain [card]. *)
let rec update_players card player = function 
  | [] -> []
  | p::ps -> if  p = player then
      let new_p = update_hand card player in
      match compare21 new_p with
      | 1 -> (Player.bust new_p)::update_players card player ps
      | _ -> new_p::(update_players card player ps)
    else p::(update_players card player ps) 

let rec hit game player =
  try
    let card, deck' = Deck.draw game.deck in
    { players = update_players card player game.players;
      deck = deck' }
  with Empty ->
    hit ({ game with deck = Deck.shuffle (Deck.make ());}) player

let rank_players game = 
  let dealer_score = get_dealer_score game in
  let won = List.filter (
      fun n -> (Player.is_playing n) && (not (Player.is_playing (
          get_player 0 game)) || (Player.get_score n > dealer_score))) in
  let tied = List.filter (
      fun n -> (not (Player.is_dealer n)) && (Player.is_playing n) && (
          Player.get_score n = dealer_score)) in
  let lost lst = List.filter 
      (fun n -> (not (List.mem n lst )) && (not (is_dealer n))) in
  let a = won (players game) in 
  let b = tied (players game) in 
  a, b, (lost (a@b) (players game))

(** [to_json g] is a representation of the players of [g] as a json list. 
    Example:  

    if g.players = [
                  { id: 0;
                    name: "Dealer";
                    hand: [];
                    score: 0; 
                    is_dealer: true;
                    is_playing: true; 
                    bet = { player_bet = 10;
                            wallet = 500;
                            min_bet = 5; 
                            max_bet = 15
                    }

                  }
    ]

    then (to_json g) = { 
                        "players": [
                          {
                            "id": 0,
                            "name": "Dealer",
                            "is_dealer": true,
                            "wallet": 500
                          }
                        ]
                      }

*)
let to_json g = 
  let player_lst = g.players in 
  let ids = List.map get_id player_lst in 
  let names = List.map get_name player_lst in
  let is_dealer = List.map is_dealer player_lst in 
  let is_cpu = List.map is_cpu player_lst in
  let bet_lst = List.map Player.get_bet player_lst in
  let wallet = List.map get_wallet bet_lst in

  let rec make_player_json p_acc ids names is_dealers is_cpus wallets = 
    match ids, names, is_dealers, is_cpus, wallets with
    | [], [], [], [], [] -> List.rev p_acc
    | i::is, n::ns, d::ds, c::cs, w::ws -> 
      let saved_p = `Assoc [("id", `Int i);
                            ("name", `String n);
                            ("is_dealer",`Bool d);
                            ("is_cpu", `Bool c);
                            ("wallet", `Int w);] in 
      make_player_json (saved_p::p_acc) is ns ds cs ws
    |_ -> 
      failwith "lists not the same length in make player json (blackjack.ml)" in

  let json_player_lst = make_player_json [] ids names is_dealer is_cpu wallet in 
  `Assoc [("players", `List json_player_lst)]

let save_state g s = 
  Yojson.to_file s (to_json g)

(** [default_bet w] is a bet with min value set to 5, max value set to 100, and
    wallet ammount set to [w]. *)
let default_bet w = 
  Bet.init w 5 100

(** [player_helper json_p] is a player with id, name, is_dealer, and bet set
    to be the values saved in [json_p]. *)
let player_helper json_p = 
  let id     = json_p |> member "id"        |> to_int in
  let name   = json_p |> member "name"      |> to_string in
  let dealer = json_p |> member "is_dealer" |> to_bool in
  let cpu    = json_p |> member "is_cpu"    |> to_bool in
  let bet    = json_p |> member "wallet"    |> to_int |> default_bet in
  if cpu then Player.init_cpu id name [] dealer bet
  else Player.init id name [] dealer bet

(** [each_draw_two g] is [g] with each player's hand initialized to contain
    two cards. *)
let each_draw_2 g = 
  let rec for_each g = function
    | [] -> g
    | p::ps ->
      let p_id = get_id p in
      let g' = hit g p in
      let g'' = hit g' (get_player p_id g') in
      for_each g'' ps in
  for_each g g.players

let load_state json = 
  { players = json |> member "players" |> to_list |> List.map player_helper;
    deck = Deck.shuffle (Deck.make ()) } 
  |> each_draw_2 

let update_wallets g = 
  let w, t, l = rank_players g in 
  let multiplier p = 
    if List.mem p w then 2
    else if List.mem p t then 1
    else 0 in 
  let new_players = List.map (
      fun p -> Player.payout (multiplier p) p ) g.players in
  { g with players = new_players }

let reset g = 
  let new_players = List.map reset_player g.players  in
  each_draw_2 { g with players = new_players }

let bet game player n =
  let new_players = List.map (
      fun p -> if p = player then Player.place_bet n p else p) 
      game.players in
  { game with players = new_players }

(** [not_enough_chips p] prints that player [p] does not have enough chips to 
    stay at their current table. *)
let not_enough_chips p = ANSITerminal.(print_string [yellow] (
    "\n" ^ Player.get_name p ^ ", sorry you do not have enough chips to pay " ^
    "for the minimum bet for this table.\n" ^ 
    "You must switch tables to keep playing.\n")
  )

(** [out_of_chips p] prints that player [p] does not have enough chips to 
    continue playing at any game. *)
let out_of_chips p = ANSITerminal.(print_string [red] (
    "\n" ^ Player.get_name p ^ ", you have run out of chips. " ^
    "You can restart this mode or start a new one.\n"))

let remove_players game =
  let _ = List.map (fun p -> 
      if Bet.get_wallet (Player.get_bet p) < 5 then out_of_chips p else
      if Bet.get_wallet (Player.get_bet p) < Bet.get_min (Player.get_bet p) 
      then not_enough_chips p else ()) game.players in
  { game with players = List.filter (
        fun p -> Bet.get_wallet (Player.get_bet p) >= 5) game.players }

let remove_player game p = 
  { game with players = List.filter (fun p' -> p <> p') game.players }

let need_to_switch game =
  List.filter (fun p ->
      not (is_dealer p) &&
      Bet.get_wallet (get_bet p) < Bet.get_min (get_bet p)) game.players
  |> List.length > 0

let remove_under_min game =
  { game with players = List.filter (fun p ->
        Bet.get_wallet (Player.get_bet p) >= Bet.get_min (Player.get_bet p)) 
        game.players }

(** [n_players n] returns a list of n human players.
    Requires: n is greater than or equal to zero. *)
let rec n_players n names acc = 
  let bet = Bet.init 100 5 100 in
  match names with
  | [] -> acc
  | name::names -> 
    (n_players (n-1) names ((Player.init n name [] false bet)::acc))

(** [n_players n] returns a list of n computer players and a dealer (Player 0).
    Requires: n is greater than or equal to zero. *)
let rec n_computers n names acc =
  let bet = Bet.init 100 5 100 in
  let d_bet = Bet.init 100 5 100 in
  match names with
  | [] -> acc@[Player.init 0 "Dealer" [] true d_bet]
  | name::names -> 
    (n_computers (n+1) names ((Player.init_cpu (n+1) name [] false bet)::acc))

let init n p_lst cpu_lst =
  each_draw_2 { players = (n_players n p_lst []) @
                          (n_computers n cpu_lst []);
                deck = Deck.shuffle (Deck.make ()) }

(** [split_player acc p card1 card2 players] is [players] after player [p]'s
    hand is split into 2 hands with one card each. One hand will contain
    [card1] and the other hand will contain [card2]. *)
let rec split_player acc p card1 card2 = function
  |[] -> acc
  |h::t -> if h = p then
      let split_p1 = Player.init (Player.get_id p) 
          (Player.get_name p) [card1] false (Player.get_bet p) in
      let split_p2 = Player.init (-(Player.get_id p))
          ((Player.get_name p) ^ " (second hand)" ) [card2] 
          false (Bet.set_temp_bet (Player.get_bet p)) in 
      split_p1::split_p2::t else
      split_player (p::acc) p card1 card2 t

(** [split_cpu acc p card1 card2 players] is the same as [split_player]
    except [split_cpu] splits a computer's hand. *)
let rec split_cpu acc p card1 card2 = function
  |[] -> acc
  |h::t -> if h = p then
      let split_p1 = Player.init_cpu (Player.get_id p) 
          (Player.get_name p) [card1] false (Player.get_bet p) in
      let split_p2 = Player.init_cpu (-(Player.get_id p))
          ((Player.get_name p) ^ " (second hand)" ) [card2] 
          false (Bet.set_temp_bet (Player.get_bet p)) in 
      split_p1::split_p2::t else
      split_player (p::acc) p card1 card2 t

let split game p card1 card2 =  
  if Player.is_cpu p then  
    let split_players = split_cpu [] p card1 card2 game.players in
    { game with players = split_players }
  else
    let split_players = split_player [] p card1 card2 game.players in
    { game with players = split_players }

let clean_after_split g = 
  let rec find_pairs acc = function
    | [] -> List.rev acc
    | pa::pas when ((get_id pa) > -1) -> begin
        try let pa = merge_wallet pa (get_player (-(get_id pa)) g) in 
          find_pairs (pa::acc) pas
        with Not_found -> find_pairs (pa::acc) pas end
    | pa::pas when ((get_id pa) < 0) -> find_pairs (pa::acc) pas
    | _ -> failwith "error in clean after split" in 
  let merged_wallets = find_pairs [] g.players in

  { g with players = List.filter (fun p -> (get_id p) > -1) merged_wallets }

(** [double_player p lst acc] is a list of players [lst] updated to contain
    player [p] with a doubled bet. *)
let rec double_player p lst acc =
  match lst with 
  | [] -> acc
  | h::t -> if Player.get_id h = Player.get_id p then 
      if Player.is_cpu p then 
        let double_p = Player.init_cpu (Player.get_id p) 
            (Player.get_name p) (Player.get_hand p) false 
            (Bet.double (Player.get_bet p)) in acc@(double_p::t)
      else
        let double_p = Player.init (Player.get_id p) 
            (Player.get_name p) (Player.get_hand p) false 
            (Bet.double (Player.get_bet p)) in
        acc@(double_p::t) else double_player p t ([h]@acc)

let double game player_id =
  let p = get_player player_id game in
  let updated_players = double_player p game.players [] in 
  let new_game = {game with players = updated_players} in 
  let new_p = get_player player_id new_game in
  hit new_game new_p

let change_table min max g = 
  let f p = p |> Player.get_bet 
            |> Bet.set_max_min max min
            |> Player.set_bet p in
  let new_players = g.players |> List.map f in
  { players = new_players;
    deck = Deck.shuffle (Deck.make()) }
