open Blackjack
open Strings
open Deck
open Formatc
open Player
open Command
open Bet
open Computer

let get_char () = 
  let a = Unix.tcgetattr Unix.stdin in 
  let () = 
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { a with Unix.c_icanon = false } in 
  let res = input_char stdin in 
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN a; res

let print_cards hand =
  let rec print_cards_h m = function
    | [] -> print_endline ""
    | c::h -> 
      let s = string_of_card_idx (Deck.index_of_card c) in
      ANSITerminal.move_cursor  (7*m) (-6);
      ANSITerminal.(print_string [Bold] ((List.nth s 0) ^ "\n"));
      ANSITerminal.move_cursor  (7*m) 0;
      ANSITerminal.(print_string [Bold] ((List.nth s 1) ^ "\n"));
      ANSITerminal.move_cursor  (7*m) 0;
      ANSITerminal.(print_string [Bold] ((List.nth s 2) ^ "\n"));
      ANSITerminal.move_cursor  (7*m) 0;
      ANSITerminal.(print_string [Bold] ((List.nth s 3) ^ "\n"));
      ANSITerminal.move_cursor  (7*m) 0;
      ANSITerminal.(print_string [Bold] ((List.nth s 4) ^ "\n"));
      ANSITerminal.move_cursor  (7*m) 0;
      ANSITerminal.(print_string [Bold] ((List.nth s 5) ^ "\n"));
      print_cards_h (m+1) h in 
  print_string "\n\n\n\n\n\n";
  print_cards_h 0 hand

(** [quit ()] asks the user if they want to quit the game and exits the game
    if they enter 'y'. *)
let quit unit = print_endline quit_m; 
  match get_char () with
  | 'y' | '\n' -> print_endline (Strings.quit); exit 0
  | _ -> ()

(** [incorrect_move hints] asks the user if they want to continue with a move
    that is different from the computer ai. *)
let rec incorrect_move hints c =
  if not hints then let () = ANSITerminal.(
      print_string [red] (Strings.incorrect_move c)) in true 
  else
    let () = ANSITerminal.(print_string [yellow] not_ai_move) in
    print_newline ();
    match get_char () with
    |'c' -> true
    |'d' -> false
    | _ -> ANSITerminal.(print_string [red] not_c_or_d); incorrect_move hints c

(* [bust card unit] prints a string on the terminal letting the player know
   their score has exceeded 21. *)
let print_bust card = ANSITerminal.(print_string [red] (
    "\nSorry, you drew a " ^ (Deck.card card) ^ 
    ". You've BUSTED! You're out of the game.\n" ))

(** [cards_to_string lst] is a string representation of all the cards in lst. *)
let rec string_of_cards = function
  |[] -> ""
  |h::[] -> Deck.card h
  |h::t -> Deck.card h ^ ", " ^ string_of_cards t

(** [check_scores_t game] is [game] after a print statement informing the user
    whether they won, lost or tied the dealer. *)
let check_scores_t game = 
  let () = Unix.sleepf (0.75) in
  let won, tie , lost = rank_players game in
  let _ =  
    if won <> [] then ANSITerminal.(print_string [green] beat_dealer) else ();
    if tie <> [] then ANSITerminal.(print_string [yellow] tied_dealer) else ();
    if lost <> [] then ANSITerminal.(print_string [red] lost_to_dealer) else (); 
  in 
  let updated_wallets = update_wallets    game in
  let removed_temps   = clean_after_split updated_wallets in 
  let removed_players = remove_players    removed_temps in removed_players

(** [play_dealer game dealer] is an updated version of [game] after [dealer]
    their turn. *)
let rec play_dealer_t game dealer =
  if (Player.compare21 dealer = 0 && Player.number_of_cards dealer = 2) then
    let () = print_endline dealers_cards;
      print_cards (Player.get_hand dealer);
      ANSITerminal.(print_string [green] (dealer_blackjack)) 
    in check_scores_t game else
  if (Player.compare21 dealer = 0) then 
    let () = ANSITerminal.(
        print_string [green] (dealer_21)) in check_scores_t game else
    let () = print_endline dealers_cards in
    print_cards (Player.get_hand dealer);
    let () = Unix.sleepf (0.75) in
    match casino_dealer dealer with
    |Hit -> let new_game = Blackjack.hit game dealer in
      let new_player = Blackjack.get_player 0 new_game in
      if Player.is_playing new_player then
        play_dealer_t new_game new_player
      else let new_card = List.hd (Player.get_hand new_player) 
        in let () = ANSITerminal.(print_string [red] (
            "\nThe dealer drew a " ^ (Deck.card new_card) ^ " and BUSTED!\n" )) 
        in check_scores_t new_game
    |Stay -> let () = print_endline(dealer_stay); in check_scores_t game
    |_ -> failwith "computer dealer error"

(** [start_dealer_turn game] starts the dealer's turn in [game]. *)
let start_dealer_turn game =
  let dealer = Blackjack.get_player 0 game in
  print_string start_dealer_turn_m; 
  match read_line () with
  | _ -> play_dealer_t game dealer

(** [print_begin_round game p] is the print statements for player [p] to
    start their move in game [game]. *)
let rec print_begin_round game p =
  if (Player.compare21 p = 0 && Player.number_of_cards p = 2) then
    let () = print_endline(dealer_top_card);
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards [top_dealer_card game] ^ ").\n"));
      print_cards [top_dealer_card game];
      print_endline("\n" ^ (Player.get_name p) ^ ", your cards are:");
      ANSITerminal.(print_string [blue] (
          "(" ^ string_of_cards (Player.get_hand p) ^ ").\n"));
      print_cards (get_hand p);
      ANSITerminal.(print_string [green] (got_blackjack)) 
    in true else
  if (Player.compare21 p = 0) then 
    let () = ANSITerminal.(
        print_string [green] ("Congratulations you drew a " ^ Deck.card (
            List.hd (Player.get_hand p)) ^ " and got 21!\n")) in true 
  else
  if (Player.compare21 p = 1) then
    let () = print_endline("\n" ^ (Player.get_name p) ^ ", your cards are:") in
    let () = ANSITerminal.(
        print_string [red] ("\nSorry, you drew a " ^ Deck.card (
            List.hd (Player.get_hand p)) ^ " and busted!\n" )) in false 
  else 
    let () = print_endline(dealer_top_card) in
    ANSITerminal.(print_string [cyan] (
        "(" ^ string_of_cards [top_dealer_card game] ^ ").\n"));
    print_cards [top_dealer_card game];
    let () = print_endline("\n" ^ (Player.get_name p) ^ ", your cards are:")
    in ANSITerminal.(print_string [blue] (
        "(" ^ string_of_cards (Player.get_hand p) ^ ").\n"));
    print_cards (get_hand p); false 

(** [guided_mode game p_id] is a player with id [p_id] playing a guided mode
    of blackjack. *)
let rec guided_mode_start game p_id hints =

  let rec quit unit = print_endline quit_m; 
    match get_char () with
    | 'y' -> print_endline (Strings.quit); true
    | 'n' -> false
    | _ -> ANSITerminal.(print_string [red] not_y_or_n); quit ()
  in

  let rec get_bets game p_id hints =
    let p = Blackjack.get_player p_id game in
    let min = Bet.get_min (Player.get_bet p) in
    let max = Bet.get_max (Player.get_bet p) in
    let wallet = Bet.get_wallet (Player.get_bet p) in
    let () = if hints then 
        ANSITerminal.(print_string [blue] (card_count_m game)) else () in
    print_string (get_bet_m p wallet);
    match read_line () with
    | "quit" -> if quit () then exit 0 else get_bets game p_id hints
    | "leave" -> begin 
        let () = if hints then 
            ANSITerminal.(print_string [blue] (leave_guided_mode)) else
            ANSITerminal.(print_string [blue] (leave_yourself_mode)) in
        let rec rem_all g = function
          | [] -> g
          | p::ps -> rem_all (remove_player g p) ps in 
        rem_all game (players game) end
    | n -> (match int_of_string n with
        |exception _ ->  
          ANSITerminal.(print_string [red] (bet_non_number));
          get_bets game p_id hints
        |n when n < min  || n > max -> 
          ANSITerminal.(
            print_string [red] (bet_outside_table_max_min min max));
          get_bets game p_id hints
        |n when n > wallet -> 
          ANSITerminal.(print_string [red] (bet_too_high));
          get_bets game p_id hints
        |n -> Blackjack.bet game p n)
  in

  let rec guided_mode_move game p_id hints =
    let p = Blackjack.get_player p_id game in
    let () = if hints then
        print_endline (p_stats game p) else () in
    match print_begin_round game p with
    |true -> start_dealer_turn game
    |false -> let () = if hints then print_string(make_move_guided) else 
                  print_string(make_move_yourself) in
      match read_line () with
      |"hint" -> 
        if hints then
          let move = good_move game p in
          ANSITerminal.(print_string [blue;Bold] (guided_mode_hit move));
          guided_mode_move game p_id hints 
        else let () = ANSITerminal.(print_string [red] no_hints_allowed) in
          guided_mode_move game p_id hints
      |"help" -> 
        let rec continue unit = ANSITerminal.(print_string [red] (type_c)); 
          print_string ("> ");
          match read_line() with 
          |"c" -> guided_mode_move game p_id hints
          |n -> continue () in
        ANSITerminal.(print_string [magenta] (rules_and_info));  
        print_string ("> "); continue ()
      | "hit" | "h" -> begin match good_move game p with
          |Hit ->
            ANSITerminal.(print_string [green] correct_move);
            let new_game = Blackjack.hit game p in
            if Player.is_playing (Blackjack.get_player p_id new_game) then
              guided_mode_move new_game p_id hints
            else 
              let new_player = Blackjack.get_player p_id new_game in
              let new_card = List.hd (Player.get_hand new_player) 
              in let _ = print_bust new_card in game
          |_ -> if incorrect_move hints (Computer.good_move game p) then 
              let new_game = Blackjack.hit game p in
              if Player.is_playing (Blackjack.get_player p_id new_game) then
                guided_mode_move new_game p_id hints
              else 
                let new_player = Blackjack.get_player p_id new_game in
                let new_card = List.hd (Player.get_hand new_player) 
                in let _ = print_bust new_card in game
            else guided_mode_move game p_id hints
        end
      | "stay" | "s" -> begin 
          match good_move game p with
          |Stay -> 
            ANSITerminal.(print_string [green] correct_move); 
            game 
          |_ -> if incorrect_move hints (Computer.good_move game p) 
            then game
            else guided_mode_move game p_id hints
        end
      | "score" -> 
        print_string Strings.get_score;
        ANSITerminal.(print_string [red] 
                        ((string_of_int (Player.get_score p) ^ "\n"))); 
        guided_mode_move game p_id hints
      | "quit" -> if quit () then exit 0 else guided_mode_move game p_id hints
      | "split" | "sp" ->
        begin match good_move game p with
          |Split ->
            ANSITerminal.(print_string [green] correct_move); 
            let hand = (get_hand) p in
            begin match hand with 
              | c1::c2::[] when rank c1 = rank c2 -> 
                let split_game   = split game p c1 c2 in 
                let split_game'  = guided_mode_move split_game p_id hints in
                guided_mode_move split_game' (-p_id) hints
              | c1::c2::[] when (rank c1 = Ace (1) && rank c2 = Ace (11)) -> 
                let split_game   = split game p (suit c1, Ace (11)) c2 in 
                let split_game'  = guided_mode_move split_game p_id hints in
                print_endline ("Go Again:");
                guided_mode_move split_game' (-p_id) hints
              | _ -> 
                ANSITerminal.(print_string [red] (invalid_split));
                guided_mode_move game p_id hints
            end
          |_ -> if incorrect_move hints (Computer.good_move game p) then 
              let hand = (get_hand) p in
              begin match hand with 
                | c1::c2::[] when rank c1 = rank c2 -> 
                  let split_game   = split game p c1 c2 in 
                  let split_game'  = guided_mode_move split_game p_id hints in
                  guided_mode_move split_game' (-p_id) hints
                | c1::c2::[] when (rank c1 = Ace (1) && rank c2 = Ace (11)) -> 
                  let split_game   = split game p (suit c1, Ace (11)) c2 in 
                  let split_game'  = guided_mode_move split_game p_id hints in
                  guided_mode_move split_game' (-p_id) hints
                | _ -> 
                  ANSITerminal.(print_string [red] (invalid_split));
                  guided_mode_move game p_id hints
              end
            else guided_mode_move game p_id hints
        end
      | "double" | "d" -> begin 
          match good_move game p with
          | Double -> ANSITerminal.(print_string [green] correct_move); 
            begin
              let bet = Player.get_bet p in
              match Bet.double bet with
              | exception Out_of_bounds -> 
                ANSITerminal.(print_string [red] (
                    "\nYou do not have enough chips " 
                    ^ "to double your bet! You only have " ^ 
                    string_of_int (Bet.get_wallet bet) ^ " chips.")); 
                guided_mode_move game p_id hints
              | _ ->
                let new_game = Blackjack.double game p_id in
                let new_p = Blackjack.get_player p_id new_game in
                ANSITerminal.(print_string [blue] (
                    "\nYour new bet amount for this round is " ^ string_of_int (
                      Bet.get_bet (Player.get_bet new_p) * 2) ^ "\n")); 
                let _ = print_begin_round new_game new_p in
                new_game 
            end
          | _ -> if incorrect_move hints (Computer.good_move game p) then 
              begin
                let bet = Player.get_bet p in
                match Bet.double bet with
                | exception Out_of_bounds -> 
                  ANSITerminal.(print_string [red] (
                      "\nYou do not have enough chips " 
                      ^ "to double your bet! You only have " ^ 
                      string_of_int (Bet.get_wallet bet) ^ " chips.")); 
                  guided_mode_move game p_id hints
                | _ ->
                  let new_game = Blackjack.double game p_id in
                  let new_p = Blackjack.get_player p_id new_game in
                  ANSITerminal.(print_string [blue] (
                      "\nYour new bet amount for this round is " ^ 
                      string_of_int (Bet.get_bet (Player.get_bet new_p) * 2)
                      ^ "\n")); 
                  ignore(print_begin_round new_game new_p); 
                  new_game 
              end
            else guided_mode_move game p_id hints
        end
      | "leave" -> begin 
          let rec rem_all g = function
            |[] -> g
            |p::ps -> rem_all (remove_player g p) ps in
          let () = if hints then
              ANSITerminal.(print_string [blue] (leave_guided_mode)) else
              ANSITerminal.(print_string [blue] (leave_yourself_mode)) in
          rem_all game (players game)
        end
      | _ -> 
        print_endline bad_command;
        guided_mode_move game p_id hints
  in

  let p = Blackjack.get_player p_id game in
  let bets = get_bets game p_id hints in
  if List.length (players bets) = 0 then bets else 
    let () = if hints then 
        print_endline (guided_mode_turn_m (Player.get_name p))
      else print_endline (yourself_mode_turn_m (Player.get_name p)) in
    let g = guided_mode_move bets p_id hints in 
    if List.length (players g) <> 0 then start_dealer_turn g else g

(** [replay game p_id] prompts a user with id [p_id] to play another round
    of game [game]. *)
let rec replay game p_id hints =
  let num_players = List.length (players game)  in
  if num_players = 0 then game else
  if num_players = 1 then 
    let () = ANSITerminal.(print_string [red] out_of_chips) in game else
    let () = if hints then print_endline guided_mode_end_of_round
      else print_endline yourself_mode_end_of_round in
    match get_char () with
    |'p' -> 
      let g = guided_mode_start (reset game) p_id hints in replay g p_id hints
    |'s' -> game
    | _  -> ANSITerminal.(print_string [red] not_command_m); 
      replay game p_id hints

(** [get_player_name ()] prompts a user to enter their name. *)
let rec get_player_name unit = 
  print_string get_name_m;
  match read_line () with
  | "q" | "quit" -> quit (); get_player_name ()
  | "" -> ANSITerminal.(print_string [red] no_name_m); get_player_name ()
  | name -> name

(** [get_player_bet g] prompts a user in game [g] to bet in learning mode. *)
let rec get_player_bet g =
  let p = get_player 1 g in 
  let b = p |> Player.get_bet in
  let w = b |> Bet.get_wallet in 
  let min = b |> Bet.get_min in
  let max = b |> Bet.get_max in
  print_endline (wallet_m w);
  print_string "> ";
  match read_line () with
  | "q" | "quit" -> quit (); get_player_bet g
  | n -> begin
      match int_of_string n with
      | exception _ ->  
        ANSITerminal.(print_string [red] (bet_non_number));
        get_player_bet g
      | n when n < min  || n > max -> 
        ANSITerminal.(
          print_string [red] (bet_outside_table_max_min min max));
        get_player_bet g
      | n when n > w -> 
        ANSITerminal.(print_string [red] (bet_too_high));
        get_player_bet g
      | n -> Blackjack.bet g p n end

(** [guided_mode ()] is a guided mode training game. *)
let yourself_mode unit =
  print_endline begin_yourself_mode_m;
  let name = get_player_name () in
  let game = Blackjack.init 1 [name] [] in 
  let round = guided_mode_start game 1 false in 
  if (List.length (players game) = 1) then
    let () = ANSITerminal.(print_string [red] out_of_chips) in round 
  else if (List.length (Blackjack.players game) <= 1) then 
    round else replay round 1 false


(** [guided_mode ()] is a guided mode training game. *)
let guided_mode unit =
  print_endline begin_guided_mode_m;
  let name = get_player_name () in
  let game = Blackjack.init 1 [name] [] in 
  let round = guided_mode_start game 1 true in 
  if (List.length (players game) = 1) then  
    let () = ANSITerminal.(print_string [red] out_of_chips) in round 
  else if (List.length (Blackjack.players game) <= 1) then 
    round else replay round 1 true

(** [play_round game] is a round of [game] played in learning mode. *)
let play_round game = 

  let rec play_round_h game name id cc add =

    ANSITerminal.(erase Below);
    let adv = string_of_float (Computer.p_advantage game) in
    let p = get_player id game in
    ANSITerminal.(print_string [magenta] (
        (p_advantage_m adv) ^ (cc_m cc) ^ (c_value_m add) ^ (cur_cc (cc + add))
        ^ (score_m (get_score p))));
    print_endline dealer_top_card;
    print_cards [top_dealer_card game];
    print_endline("\n" ^ name ^ ", your cards are:");
    print_cards (p |> get_hand);

    match compare21 p with
    |  0 -> ANSITerminal.(print_string [green] got_blackjack); game
    |  1 -> 
      let bust_card = List.hd (Player.get_hand p) in 
      print_bust bust_card;
      Unix.sleepf(0.5); game
    |  _ -> 
      let sd = command_to_string (good_move game p) in
      ANSITerminal.(print_string [magenta]  (p_bust_m p ^ sd_move_m sd));
      print_string make_move_m; begin
        match read_line () with 
        | "help" | "p" -> 
          print_endline tutorial_help_m;
          play_round_h game name id cc add
        | "hit"  | "h" ->
          ANSITerminal.move_cursor (-1) (-28); 
          let game' = Blackjack.hit game p in
          let p' = get_player 1 game' in
          let card = List.hd (Player.get_hand p') in
          let cc' = cc + add in
          let add = (wong_halves_count 0 [number_of_card card]) in
          play_round_h game' name id cc' add 
        | "stay" | "s" -> game
        | "double" | "d" -> 
          let hand = (get_hand) p in
          begin match hand with 
            | c1::c2::[] when score 0 [c1;c2] >= 9 && score 0 [c1;c2] <= 11 -> 
              let bet = Player.get_bet p in
              begin
                match Bet.double bet with
                | exception Out_of_bounds -> 
                  let () =  ANSITerminal.(print_string [red] (
                      "\nYou do not have enough chips " 
                      ^ "to double your bet! \nYou only have " ^ 
                      string_of_int (Bet.get_wallet bet) ^ " chips.")) in
                  let () = print_newline () in 
                  let () = Unix.sleep(3) in
                  let () = ANSITerminal.move_cursor (-1) (-31) in
                  play_round_h game name id cc add
                | _ ->
                  let () = ANSITerminal.(print_string [blue] (
                      "\nYour new bet amount for this round is " ^ 
                      string_of_int (Bet.get_bet (Player.get_bet p) * 2) ^ "\n" 
                      ^ "After you Double the dealer gives you a card" ^
                      " and you end your turn... \n")) in 
                  let () = print_newline () in 
                  let () = Unix.sleepf(3.5) in
                  let game' = Blackjack.double game id in
                  let () = ANSITerminal.move_cursor (-1) (-32) in
                  ANSITerminal.(erase Below);
                  let adv = string_of_float (Computer.p_advantage game') in
                  let p = get_player id game' in
                  ANSITerminal.(print_string [magenta] (
                      (p_advantage_m adv) ^ (cc_m cc) ^ (c_value_m add) ^ 
                      (cur_cc (cc + add)) ^ (score_m (get_score p))));
                  print_endline dealer_top_card;
                  print_cards [top_dealer_card game'];
                  print_endline("\n" ^ name ^ ", your cards are:");
                  print_cards (p |> get_hand);
                  game'
              end
            | _ -> let () =  ANSITerminal.(print_string [red] (
                "\nYou can only double when your hand has 2 cards and your" ^
                "\ntotal sore is 9, 10, or 11.")) in
              let () = print_newline () in 
              let () = Unix.sleep(3) in
              let () = ANSITerminal.move_cursor (-1) (-31) in
              play_round_h game name id cc add
          end
        | "split" | "sp" -> 
          let hand = (get_hand) p in
          begin match hand with 
            | c1::c2::[] when rank c1 = rank c2 -> 
              let split_game   = split game p c1 c2 in 
              let split_game'  = 
                let () = ANSITerminal.move_cursor (-1) (-28) in
                play_round_h split_game name (-id) cc add in
              print_endline ("Go Again :");
              play_round_h split_game' name id cc add
            | c1::c2::[] when (rank c1 = Ace (1) && rank c2 = Ace (11)) -> 
              let split_game   = split game p (suit c1, Ace (11)) c2 in 
              let () = ANSITerminal.move_cursor (-1) (-28) in
              let split_game'  = play_round_h split_game name (-id) cc add in
              print_endline ("Go Again :");
              play_round_h split_game' name id cc add
            | _ -> 
              let () = ANSITerminal.(print_string [red] ("\n"^invalid_split)) in
              let () = print_newline () in
              let () = Unix.sleepf(1.5) in
              let () = ANSITerminal.move_cursor (-1) (-31) in
              play_round_h game name id cc add
          end
        | "quit" | "q" -> quit (); 
          ANSITerminal.move_cursor (-1) (-32);
          play_round_h game name id cc add
        | "leave" -> 
          let rec rem_all g = function
            | [] -> g
            | p::ps -> rem_all (remove_player g p) ps in 
          rem_all game (players game)
        | "\n"-> 
          ANSITerminal.move_cursor (-1) (-26);
          play_round_h game name id cc add
        |  _  -> 
          ANSITerminal.move_cursor (-1) (-28);
          play_round_h game name id cc add
      end 
  in 

  let name = game |> get_player 1 |> get_name in 
  let cc = update_cc game in
  ANSITerminal.(print_string [blue] (header_m name));
  print_newline ();
  play_round_h game name 1 cc 0

(** [learning_mode ()] creates a learning mode training game. *)
let rec learning_mode unit = 

  let rec learning_mode_h game name round = 
    let p = game |> get_player 1 in

    let () = if round <> 0 then
        let () = ANSITerminal.(print_string [Bold; green] (new_round_m)) in
        print_newline ();
        let suggested = Computer.card_counter game p in
        let cc = update_cc game in
        ANSITerminal.(print_string [green] (bet_m cc suggested));
      else () in

    let game' = game 
                |> get_player_bet
                |> play_round in

    if List.length (players game') <> 0 then
      let game'' = game'
                   |> start_dealer_turn
                   |> clean_after_split in
      let () = print_endline play_again_m in 
      match get_char () with
      | 'e' -> ()
      | 'q' -> quit (); learning_mode_h game'' name 1 
      |  _ -> 
        try learning_mode_h (reset game'') name 1 
        with Not_found -> learning_mode ();
    else () in

  print_endline begin_learning_mode_m;
  let name = get_player_name () in
  let game = Blackjack.init 1 [name] [] in 
  print_endline (place_bet_m name); 
  let min = get_player 1 game |> Player.get_bet |> Bet.get_min in
  print_endline (bet_buy_in_m min);
  learning_mode_h game name 0

(** [menu ()] is the training mode game optinos. *)
let rec menu unit = 
  print_endline welcome_m;
  match get_char () with
  | 'l' -> ANSITerminal.(print_string [Bold; blue] learning_mode_m); 
    learning_mode (); menu ()
  | 'g' -> ANSITerminal.(print_string [Bold; blue] guided_mode_m); 
    ignore(guided_mode ()); menu ()
  | 'y' -> ANSITerminal.(print_string [Bold; blue] yourself_mode_m); 
    ignore(yourself_mode ()); menu ()
  | 'e' -> ()
  | 'q' -> quit (); menu ()
  |  _  -> print_endline not_command_m; menu ()

let init unit = menu ()
