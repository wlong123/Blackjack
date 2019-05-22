open Bet 
open Blackjack
open Command
open Computer
open Deck 
open Player
open OUnit2

(*----------------------------------------------------------------------------*)
(*                                    BET                                     *)
(*----------------------------------------------------------------------------*)

let bet1_init = Bet.init 1000 0 50
let player1_bet1 = Bet.place_bet 3 bet1_init 
let player1_bet2 = Bet.double player1_bet1
let bet2_init = Bet.set_max_min 1000 100 bet1_init
let place_bet3 = Bet.place_bet 100 bet2_init
let place_bet4 = Bet.double place_bet3
let bet3 = Bet.init 125 25 500
let bet3_temp = Bet.set_temp_bet bet3
let add_wallets_bet3 = Bet.add_wallets bet3 bet3_temp

let bet_tests = [
  "reset bet" >:: (
    fun _ -> assert_equal 0 (Bet.get_bet (Bet.reset_bet bet1_init)));
  "get wallet bet1" >:: (fun _ -> assert_equal 1000 (Bet.get_wallet bet1_init));
  "get wallet bet3" >:: (fun _ -> assert_equal 125 (Bet.get_wallet bet3));
  "get bet1" >:: (fun _ -> assert_equal 3 (Bet.get_bet player1_bet1));
  "get bet3" >:: (fun _ -> assert_equal 100 (Bet.get_bet place_bet3));
  "Doubled bet1" >:: (fun _ -> assert_equal 6 (Bet.get_bet player1_bet2));
  "Doubled bet3" >:: (fun _ -> assert_equal 200 (Bet.get_bet place_bet4));
  "get min bet1" >:: (fun _ -> assert_equal 0 (Bet.get_min bet1_init));
  "get min set min" >:: (fun _ -> assert_equal 100 (Bet.get_min bet2_init));
  "get min bet3" >:: (fun _ -> assert_equal 25 (Bet.get_min bet3));
  "get max bet1" >:: (fun _ -> assert_equal 50 (Bet.get_max bet1_init));
  "get max set max" >:: (fun _ -> assert_equal 1000 (Bet.get_max bet2_init));
  "get max bet3" >:: (fun _ -> assert_equal 500 (Bet.get_max bet3));
  "update wallet bet1" >:: (fun _ -> assert_equal 1003 (
      Bet.get_wallet (Bet.update_wallet 2 player1_bet1)));
  "update wallet bet3" >:: (fun _ -> assert_equal 994 (
      Bet.get_wallet (Bet.update_wallet 0 player1_bet2)));
  "set_temp_bet bet3 min" >:: (fun _ -> 
      assert_equal (get_min bet3) (get_min bet3_temp) );
  "set_temp_bet bet3 max" >:: (fun _ -> 
      assert_equal (get_max bet3) (get_max bet3_temp) );
  "set_temp_bet bet3 wallet" >:: (fun _ -> 
      assert_equal (-(Bet.get_bet bet3)) (get_wallet bet3_temp) ); 
  "add_wallets bet3" >:: (fun _ -> 
      assert_equal 125 (get_wallet add_wallets_bet3));
]

(*----------------------------------------------------------------------------*)
(*                                BLACKJACK                                   *)
(*----------------------------------------------------------------------------*)

let zero_players = Blackjack.init 0 [] []
let game = Blackjack.init 3 ["a";"b";"c"] []
let hit = Blackjack.hit game (Blackjack.get_player 2 game)
let hit_2 = Blackjack.hit hit (Blackjack.get_player 2 hit)
let dealer_hit = Blackjack.hit game (Blackjack.get_player 0 game)
let reset_game = Blackjack.reset game
let players = Blackjack.players game
let reset_players = Blackjack.players reset_game
let loaded_game = load_state (Yojson.Basic.from_file "do_not_change.json")
let lg_players = Blackjack.players loaded_game
let lg_p_amt = List.length lg_players
let lg_p_wallets = List.map (fun p -> p |> get_bet |> get_wallet) lg_players
let lg_fst_p = List.hd lg_players
let bet_10 = Blackjack.bet loaded_game lg_fst_p 10
let bet_20 = Blackjack.double bet_10 1
let p_b_20 = Blackjack.get_player 1 bet_20
             |> Player.get_bet
             |> Bet.get_bet 
let lg_fst_p' = bet_10 |> Blackjack.players |> List.hd 
let lg_fst_p_bet = lg_fst_p' |> Player.get_bet |> Bet.get_bet
let tail = lg_fst_p :: (List.tl (List.tl lg_players))
(* The following line causes a print statement; threfore it was commented out
   after being tested. *)
(* let lg_fst_rem = loaded_game |> remove_players |> Blackjack.players *)
let lg_double_fst = Blackjack.double bet_10 1
let lg_fst_bet_post_double = lg_double_fst
                             |> Blackjack.players 
                             |> List.hd 
                             |> Player.get_bet 
                             |> Bet.get_bet

(* The following funcitons cannot be tested due to randomization:
    - get_deck
    - get_dealer_score
    - rank_players
    - top_dealer_card
    - update_wallets
    - split

   The funcitons were play tested with corner cases including:
   Aces, Resets, Empty deck, Wins, Ties, Losses 

   The following functions were tested by inspection
    - save_state
*)

let string_of_int_list lst =
  let rec helper acc = function
    | [] -> acc
    | h::t -> helper (acc^";"^(string_of_int h)) t in
  helper "" lst

let blackjack_tests = [
  "not found player" >:: (
    fun _ -> assert_raises (Not_found) (
        fun () -> Blackjack.get_player 5 game));
  "found player 2" >:: (
    fun _ -> assert_equal 2 (Player.get_id (Blackjack.get_player 2 game)));
  "not found player get score" >:: (
    fun _ -> assert_raises (Not_found) (
        fun () -> Blackjack.get_score 5 game));
  "zero player game" >:: (
    fun _ -> assert_equal 1 (List.length (Blackjack.players zero_players)));
  "three player game" >:: (
    fun _ -> assert_equal 4 (List.length (Blackjack.players game)));
  "is player true" >:: (
    fun _ -> assert (Blackjack.is_player (Blackjack.get_player 2 game) game));
  "is player false" >:: (
    fun _ -> assert (
      not (Blackjack.is_player (Blackjack.get_player 2 game) zero_players)));
  "reset game generates new scores" >:: (
    fun _ -> assert (
      not (List.map get_score players = List.map get_score reset_players)));
  "reset game generates new hands" >:: (
    fun _ -> assert (
      not (List.map get_hand players = List.map get_hand reset_players)));
  "load game num of players" >:: (
    fun _ -> assert_equal 4 lg_p_amt);
  "load game player wallets" >:: (
    fun _ -> assert_equal [50; 0; 50; 4000] lg_p_wallets 
        ~printer:string_of_int_list);
  "set bet of first player" >:: (
    fun _ -> assert_equal 10 lg_fst_p_bet);
  (* "remove first player (THIS TEST CASE CAUSES A STATEMENT TO BE PRINTED)" 
      >:: (fun _ -> assert_equal tail lg_fst_rem); *)
  "set bet of first player" >:: (
    fun _ -> assert_equal 20 lg_fst_bet_post_double);
  "double bet" >:: (
    fun _ -> assert_equal 20 p_b_20 ~printer:string_of_int);
]

(*----------------------------------------------------------------------------*)
(*                                  COMMAND                                   *)
(*----------------------------------------------------------------------------*)

let command_tests = [
  "empty command" >:: (
    fun _ -> assert_raises (Command.Empty) (fun () -> Command.parse "   "));
  "hit" >:: (fun _ -> assert_equal Hit (Command.parse "  hit  "));
  "stay" >:: (fun _ -> assert_equal Stay (Command.parse "  stay  "));
  "score" >:: (fun _ -> assert_equal Score (Command.parse "  score  "));
  "quit" >:: (fun _ -> assert_equal Quit (Command.parse "  quit  "));
  "double" >:: (fun _ -> assert_equal Double (Command.parse "  double  "));
  "split" >:: (fun _ -> assert_equal Split (Command.parse "  split  "));
  "load" >:: (fun _ -> assert_equal Load (Command.parse "  load  "));
  "save" >:: (fun _ -> assert_equal Save (Command.parse "  save  "));
  "malformed word" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " bad  "));
  "malformed hit" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " hit 123  "));
  "malformed stay" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " stay now  "));
  "malformed score" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " my   score  "));
  "malformed quit" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " quit  now  "));
  "malformed double" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " double 123  "));
  "malformed split" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " split now  "));
  "malformed load" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " load   score  "));
  "malformed save" >:: (fun _ -> assert_raises (Malformed) (
      fun () -> Command.parse " save  now  "));
  "command to string help" >:: (fun _ -> 
      assert_equal "help" (command_to_string Help));
  "command to string hit" >:: (fun _ -> 
      assert_equal "hit" (command_to_string Hit));
  "command to string stay" >:: (fun _ -> 
      assert_equal "stay" (command_to_string Stay));
  "command to string score" >:: (fun _ -> 
      assert_equal "score" (command_to_string Score));
  "command to string help" >:: (fun _ -> 
      assert_equal "quit" (command_to_string Quit));
  "command to string help" >:: (fun _ -> 
      assert_equal "double" (command_to_string Double));
  "command to string help" >:: (fun _ -> 
      assert_equal "split" (command_to_string Split));
  "command to string help" >:: (fun _ -> 
      assert_equal "load" (command_to_string Load));
  "command to string help" >:: (fun _ -> 
      assert_equal "save" (command_to_string Save));
  "command to string help" >:: (fun _ -> 
      assert_equal "leave" (command_to_string Leave));
]

(*----------------------------------------------------------------------------*)
(*                                   DECK                                     *)
(*----------------------------------------------------------------------------*)

let deck = Deck.make ()
let shuffled = Deck.shuffle deck
let top,rest_of_deck = Deck.draw deck
let top2,rest_of_deck2 = Deck.draw rest_of_deck
let jack_club = Club,Jack
let queen_diamond = Diamond,Queen
let king_heart = Heart,King
let ace_heart = Heart,Ace 1
let five_spade = Spade,Number 5
let hand = jack_club::queen_diamond::king_heart::five_spade::[]
let ace = Spade,Ace 11

let deck_tests = [
  "size of full deck" >:: (fun _ -> assert_equal 52 (Deck.size deck));
  "shuffle deck size" >:: (fun _ -> assert_equal 52 (Deck.size shuffled));
  "shuffling changes the deck" >:: (fun _ -> assert (not (shuffled = deck)));
  "size after drawing cards" >:: (
    fun _ -> assert_equal 51 (Deck.size rest_of_deck));
  "size draw 2 cards" >:: (fun _ -> assert_equal 50 (Deck.size rest_of_deck2));
  "score of jack" >:: (fun _ -> assert_equal 10 (Deck.score 0 [jack_club]));
  "score of ace" >:: (fun _ -> assert_equal 1 (Deck.score 0 [ace_heart]));
  "score of hand" >:: (fun _ -> assert_equal 35 (Deck.score 0 hand));
  "club suite" >:: (fun _ -> assert_equal Club (Deck.suit jack_club));
  "diamond suite" >:: (
    fun _ -> assert_equal Diamond (Deck.suit queen_diamond));
  "heart suite" >:: (fun _ -> assert_equal Heart (Deck.suit king_heart));
  "spade suite" >:: (fun _ -> assert_equal Spade (Deck.suit five_spade));
  "jack rank" >:: (fun _ -> assert_equal Jack (Deck.rank jack_club));
  "queen rank" >:: (fun _ -> assert_equal Queen (Deck.rank queen_diamond));
  "king rank" >:: (fun _ -> assert_equal King (Deck.rank king_heart));
  "ace rank" >:: (fun _ -> assert_equal (Ace 1) (Deck.rank ace_heart));
  "five rank" >:: (fun _ -> assert_equal (Number 5) (Deck.rank five_spade));
  "downgrade ace" >:: (
    fun _ -> let card' = (Deck.downgrade_ace [ace]) in
      assert_equal (Spade,Ace 1) (suit (List.hd card'), rank (List.hd card')));
  "downgrade regular card" >:: (
    fun _ -> let card'' = (Deck.downgrade_ace [five_spade]) in
      assert_equal five_spade (suit (List.hd card''), rank (List.hd card'')));
  "Jack of Club card" >:: (fun _ -> assert_equal "Jack of Club" (
      Deck.card jack_club));
  "5 of Spade card" >:: (fun _ -> assert_equal "5 of Spade" (
      Deck.card five_spade));
  "index of ace of spade" >:: (
    fun _ -> assert_equal (Deck.index_of_card ace) (1, 0));
  "index of king of heart" >:: (
    fun _ -> assert_equal (Deck.index_of_card king_heart) (13, 1));
  "index of queen of diamond" >:: (
    fun _ -> assert_equal (Deck.index_of_card queen_diamond) (12, 2));
  "index of jack of club" >:: (
    fun _ -> assert_equal (Deck.index_of_card jack_club) (11, 3));
  "index of five of spade" >:: (
    fun _ -> assert_equal (Deck.index_of_card five_spade) (5, 0));
  "mem" >:: (
    fun _ -> assert_equal false (Deck.mem top rest_of_deck));
  "mem false" >:: (
    fun _ -> assert_equal false (Deck.mem top rest_of_deck));
  "mem true" >:: (
    fun _ -> assert_equal true (Deck.mem top2 rest_of_deck));
]

(*----------------------------------------------------------------------------*)
(*                                 PLAYER                                     *)
(*----------------------------------------------------------------------------*)

let bet = Bet.init 25 5 20
let player1 = Player.init 1 "Player 1" [] false bet 
let player1_hand = Player.update_hand ace_heart player1 |> update_hand jack_club

let player2 = Player.init 2 "Player 2" [] false bet
let player2_hand = Player.update_hand queen_diamond player2 
                   |> update_hand king_heart |> update_hand five_spade
let dealer = Player.init 3 "Dealer" [] true bet
let player_21 = Player.update_hand ace_heart dealer 
                |> update_hand jack_club |> update_hand jack_club
let reset = Player.reset_player player_21
let player1_bet = Player.place_bet 20 player1
let player1_win = Player.payout 2 player1_bet
let player1_tie = Player.payout 1 player1_bet
let player1_lose = Player.payout 0 player1_bet
let bust = Player.bust player1
let wallet1 = player1 |> Player.get_bet |> Bet.get_wallet
let wallet2 = player2 |> Player.get_bet |> Bet.get_wallet
let wallet12 = Player.merge_wallet player1 player2 
               |> Player.get_bet 
               |> Bet.get_wallet
let init_cpu0 = Player.init_cpu 1 "a" [] false bet

let player_tests = [
  "empty hand size" >:: (
    fun _ -> assert_equal 0 (Player.number_of_cards player1));
  "2 card hand" >:: (
    fun _ -> assert_equal 2 (Player.number_of_cards player1_hand));
  "3 card hand" >:: (fun _ -> assert_equal 3 (number_of_cards player2_hand));
  "reset player hand" >:: (fun _ -> assert_equal 0 (number_of_cards reset));
  "is dealer true" >:: (fun _ -> assert (Player.is_dealer dealer));
  "is dealer false" >:: (fun _ -> assert (not (Player.is_dealer player1)));
  "less than 21" >:: (
    fun _ -> assert_equal (-1) (Player.compare21 player1_hand));
  "equal to 21" >:: (fun _ -> assert_equal 0 (Player.compare21 player_21));
  "greater than 21" >:: (
    fun _ -> assert_equal 1 (Player.compare21 player2_hand));
  "25 score" >:: (fun _ ->  assert_equal 25 (Player.get_score player2_hand));
  "21 score" >:: (fun _ ->  assert_equal 21 (Player.get_score player_21));
  "11 score" >:: (fun _ -> assert_equal 11 (Player.get_score player1_hand));
  "score empty hand" >:: (fun _ -> assert_equal 0 (Player.get_score player1));
  "score reset hand" >:: (fun _ -> assert_equal 0 (Player.get_score reset));
  "id player 1" >:: (fun _ -> assert_equal 1 (Player.get_id player1));
  "id player 2" >:: (fun _ -> assert_equal 2 (Player.get_id player2));
  "id dealer" >:: (fun _ -> assert_equal 3 (Player.get_id dealer));
  "player 1 cards" >:: (fun _ -> assert_equal 2 (
      List.length (Player.get_hand player1_hand)));
  "player 2 cards" >:: (fun _ -> assert_equal 3 (
      List.length (Player.get_hand player2_hand)));
  "empty hand cards" >:: (fun _ -> assert_equal [] (Player.get_hand player1));
  "reset cards" >:: (fun _ -> assert_equal [] (Player.get_hand reset));
  "name player 1" >:: (
    fun _ -> assert_equal "Player 1" (Player.get_name player1));
  "name player 2" >:: (
    fun _ -> assert_equal "Player 2" (Player.get_name player2));
  "is playing true" >:: (fun _ -> assert (Player.is_playing player1));
  "is playing false" >:: (fun _ -> assert (not (Player.is_playing bust)));
  "get bet" >:: (fun _ -> assert_equal bet (Player.get_bet player1));
  "place bet" >:: (
    fun _ -> assert_equal 20 (Bet.get_bet (Player.get_bet player1_bet)));
  "below min bet" >:: (fun _ -> assert_raises (Out_of_bounds) (
      fun () -> Player.place_bet 0 player1));
  "above max bet" >:: (fun _ -> assert_raises (Out_of_bounds) (
      fun () -> Player.place_bet 200 player1));
  "place bet get wallet" >:: (
    fun _ -> assert_equal 5 (Bet.get_wallet (Player.get_bet player1_bet)));  
  "win get wallet" >:: (
    fun _ -> assert_equal 45 (Bet.get_wallet (Player.get_bet player1_win)));
  "tie get wallet" >:: (
    fun _ -> assert_equal 25 (Bet.get_wallet (Player.get_bet player1_tie)));  
  "lose get wallet" >:: (
    fun _ -> assert_equal 5 (Bet.get_wallet (Player.get_bet player1_lose))); 
  "add wallet" >:: (
    fun _ -> assert_equal (wallet1 + wallet2) wallet12); 
  "cpu" >:: (
    fun _ -> assert (Player.is_cpu init_cpu0));
]

(*----------------------------------------------------------------------------*)
(*                                 COMPUTER                                   *)
(*----------------------------------------------------------------------------*)

let zero_players = Blackjack.init 0 [] []
let six_spade = Spade,Number 6
let bet = Bet.init 25 5 20
let player1 = Player.init 1 "Player 1" [] false bet 
let player1_hand =  player1 |> Player.update_hand ace_heart  
                    |> update_hand five_spade

let player2 = Player.init 2 "Player 2" [] false bet
let player2_hand = Player.update_hand queen_diamond player2 
                   |> update_hand king_heart |> update_hand five_spade

let player3 = Player.init 3 "Player 3" [] false bet
let player3_hand = Player.update_hand queen_diamond player3 
                   |> update_hand five_spade

let dealer = Player.init 3 "Dealer" [] true bet
let player_21 = Player.update_hand ace_heart dealer 
                |> update_hand jack_club |> update_hand jack_club
let ace_ace = Player.update_hand ace_heart dealer |> update_hand ace
let eleven = Player.update_hand five_spade dealer |> update_hand six_spade
let twenty = Player.update_hand jack_club dealer |> update_hand jack_club
let zero_players = Blackjack.init 0 [] []
let cheater_h1 = Computer.cheater player1_hand five_spade
let cheater_h2 = Computer.cheater player1_hand ace_heart
let cheater_s  = Computer.cheater player2_hand queen_diamond
let wong1 = wong_halves_count 0 [rank top]
let wong2 = wong_halves_count 0 (Deck.map Deck.number_of_card rest_of_deck)

(* The following funcitons cannot be tested due to randomization:
    - card counter
    - update_cc 

    Therefore they were thoughroughly play tested by multiple people.
*)

let computer_tests = [
  "hit at 16" >:: (
    fun _ -> assert_equal Hit (Computer.casino_dealer player1_hand));
  "hit empty hand" >:: (
    fun _ -> assert_equal Hit (Computer.casino_dealer player1));
  "stay over 16" >:: (
    fun _ -> assert_equal Stay (Computer.casino_dealer player2_hand));
  "split 2 aces" >:: (
    fun _ -> assert_equal Split (Computer.good_move zero_players ace_ace));
  "double at eleven" >:: (
    fun _ -> assert_equal Double (Computer.good_move zero_players eleven));
  "stay at twenty" >:: (
    fun _ -> assert_equal Stay (Computer.good_move zero_players twenty));
  "cheater hits 1" >:: (
    fun _ -> assert_equal Hit cheater_h1);
  "cheater hits 2" >:: (
    fun _ -> assert_equal Hit cheater_h2);
  "cheater stays" >:: (
    fun _ -> assert_equal Stay cheater_s);
  "wong1" >:: (
    fun _ -> assert_equal (-2) wong1 ~printer:string_of_int);
  "wong2" >:: (
    fun _ -> assert_equal 2 wong2 ~printer:string_of_int);
  "p bust 21" >:: (fun _ -> assert_equal 100.0 (p_bust player_21));
  "p bust 15" >:: (fun _ -> assert_equal 58.0 (p_bust player3_hand));
]

let suite =
  "test suite for Blackjack"  >::: List.flatten [
    bet_tests;
    blackjack_tests;
    command_tests;
    computer_tests;
    deck_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite