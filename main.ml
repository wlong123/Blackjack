open Command
open Blackjack
open Player
open Computer
open Yojson.Basic.Util
open Formatc
open Deck
open Bet
open Unix
open Strings
open Training

(** [cards_to_string lst] is a string representation of all the cards in lst. *)
let rec string_of_cards = function
  |[] -> ""
  |h::[] -> Deck.card h
  |h::t -> Deck.card h ^ ", " ^ string_of_cards t

(* [bust card unit] prints a string on the terminal letting the player know
   their score has exceeded 21. *)
let print_bust card = ANSITerminal.(print_string [red] (
    "Sorry, you drew a " ^ (Deck.card card) ^ 
    ". You've BUSTED! You're out of the game.\n" ))

(** [print_begin_round game player_id] handles all of the print statements
    for the player with id [player_id] at the beginning of their round *)
let rec print_begin_round game player_id =
  let p = Blackjack.get_player player_id game in
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
    print_cards (get_hand p) ; false 

(** [play_turn game player] starts a turn for [player] in [game]. *)
let rec play_turn game player_id = 
  match print_begin_round game player_id with
  |true -> game
  |false ->
    print_string(make_move);
    try 
      let p = Blackjack.get_player player_id game in
      match Command.parse(read_line ()) with
      |Help -> 
        let rec continue unit  = ANSITerminal.(print_string [red] (type_c)); 
          print_string (">");
          match read_line() with 
          |"c" -> play_turn game player_id
          |n -> continue() in
        ANSITerminal.(print_string [magenta] (rules_and_info));  
        print_string (">"); continue()
      | Hit -> 
        let new_game = Blackjack.hit game p in
        if Player.is_playing (Blackjack.get_player player_id new_game) then
          play_turn new_game player_id
        else 
          let new_player = Blackjack.get_player player_id new_game in
          let new_card = List.hd (Player.get_hand new_player) 
          in let _ = print_bust new_card in new_game
      | Stay -> game
      | Score -> 
        print_string get_score;
        ANSITerminal.(print_string [red] 
                        ((string_of_int (Player.get_score p) ^ "\n"))); 
        play_turn game player_id
      | Quit -> print_endline quit; exit 0
      | Load ->
        print_endline invalid_load;
        play_turn game player_id
      | Save -> 
        print_endline invalid_save;
        play_turn game player_id
      | Split -> begin
          let hand = (get_hand) p in
          match hand with 
          | c1::c2::[] when rank c1 = rank c2 -> 
            let split_game   = split game p c1 c2 in 
            let split_game'  = play_turn split_game player_id in
            play_turn split_game' (-player_id)
          | c1::c2::[] when (rank c1 = Ace (1) && rank c2 = Ace (11)) -> 
            let split_game   = split game p (suit c1, Ace (11)) c2 in 
            let split_game'  = play_turn split_game player_id in
            play_turn split_game' (-player_id) 
          | _ -> 
            ANSITerminal.(print_string [red] (invalid_split));
            play_turn game player_id
        end
      | Double -> begin
          let bet = Player.get_bet p in
          match Bet.double bet with
          | exception Out_of_bounds -> 
            ANSITerminal.(print_string [red] (
                "\nYou do not have enough chips " 
                ^ "to double your bet! You only have " ^ 
                string_of_int (Bet.get_wallet bet) ^ " chips.\n")); 
            play_turn game player_id
          | _ ->
            let new_game = Blackjack.double game player_id in
            ANSITerminal.(print_string [blue] (
                "\nYour new bet amount for this round is " ^ string_of_int (
                  Bet.get_bet (Player.get_bet p) * 2) ^ "\n")); 
            ignore(print_begin_round new_game player_id); new_game end
      | Leave -> begin 
          ANSITerminal.(print_string [red] (leave_game));
          (Blackjack.remove_player game p) 
        end
    with exp -> 
      print_endline bad_command;
      play_turn game player_id

(* [print_cpu_begin_round game cpu_id] is the begining of a computer's round. *)
let print_cpu_begin_round game cpu_id =
  let cpu = Blackjack.get_player cpu_id game in
  let cpu_name = Player.get_name cpu in
  if (Player.compare21 cpu = 0 && Player.number_of_cards cpu = 2) then
    let () = print_endline("\n" ^ cpu_name ^ "'s cards are: ");
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards (Player.get_hand cpu) ^ ").\n"));
      print_cards (Player.get_hand cpu);
      print_endline dealer_top_card;
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards [top_dealer_card game] ^ ").\n"));
      print_cards [top_dealer_card game];
      ANSITerminal.(print_string [green] (cpu_name ^ " Got Blackjack!\n")) 
    in true else
  if (Player.compare21 cpu = 0) then 
    let () = print_endline("\n" ^ cpu_name ^ "'s cards are: ");
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards (Player.get_hand cpu) ^ ").\n"));
      print_cards (Player.get_hand cpu);
      print_endline dealer_top_card;
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards [top_dealer_card game] ^ ").\n"));
      print_cards [top_dealer_card game]; in
    let () = ANSITerminal.(
        print_string [green] (cpu_name ^ " Got 21!\n")) 
    in true else
    let () = print_endline("\n" ^ cpu_name ^ "'s cards are:"); 
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards (Player.get_hand cpu) ^ ").\n"));
      print_cards (Player.get_hand cpu);
      print_endline dealer_top_card;
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards [top_dealer_card game] ^ ").\n"));
      print_cards [top_dealer_card game]; in false

(* [play_cpu game cpu_id] is a game after a computer has played his round.
   The computer's play is always based on what will increase their chances 
   of winning based on the soft and hard stategies.  *)
let rec play_cpu game cpu_id =
  let cpu = Blackjack.get_player cpu_id game in
  let cpu_name = Player.get_name cpu in
  match print_cpu_begin_round game cpu_id with
  |true -> game
  |false ->
    let () = Unix.sleepf (2.0) in
    match Computer.good_move game cpu with
    | Hit ->
      let () = print_endline(
          "\n" ^ cpu_name ^ " has chosen to hit.\n") in 
      let new_game = Blackjack.hit game cpu in
      let new_player = Blackjack.get_player 0 new_game in
      if Player.is_playing new_player then
        let () = Unix.sleepf (1.0) in 
        play_cpu new_game cpu_id
      else let new_card = List.hd (Player.get_hand new_player) 
        in let () = ANSITerminal.(print_string [red] (
            "\n" ^ cpu_name ^ " drew a " ^ 
            (Deck.card new_card) ^ " and BUSTED!\n" )) 
        in let () = Unix.sleepf (2.0) in 
        new_game
    | Stay -> let () = print_endline(
        "\n" ^ cpu_name ^ " has chosen to stay.\n") in
      let () = Unix.sleepf (1.0) in
      game
    | Split -> begin let () = print_endline(
        "\n" ^ cpu_name ^ " has chosen to split.\n") in 
        let hand = (get_hand) cpu in
        match hand with 
        | c1::c2::[] when rank c1 = rank c2 -> 
          let split_game   = split game cpu c1 c2 in 
          let split_game'  = play_cpu split_game cpu_id in
          let () = Unix.sleepf (1.0) in
          play_cpu split_game' (-cpu_id)
        | c1::c2::[] when (rank c1 = Ace (1) && rank c2 = Ace (11)) -> 
          let split_game   = split game cpu (suit c1, Ace (11)) c2 in 
          let split_game'  = play_cpu split_game cpu_id in
          let () = Unix.sleepf (1.0) in
          play_cpu split_game' (-cpu_id) 
        | _ -> failwith "cpu split error"
      end
    | Double -> begin let () = print_endline(
        "\n" ^ cpu_name ^ " has chosen to double.\n"); in
        let bet = Player.get_bet cpu in
        match Bet.double bet with
        | exception Out_of_bounds -> game
        | _ ->
          let new_game = Blackjack.double game cpu_id in
          ANSITerminal.(print_string [blue] (
              "\nThe cpu's new bet amount for this round is " ^ string_of_int (
                Bet.get_bet (Player.get_bet cpu) * 2) ^ "\n")); 
          let _ = print_cpu_begin_round new_game cpu_id in
          let () = Unix.sleepf (1.0) in
          new_game end
    | _ -> failwith "computer error"

(** [print_string_list lst] is a string representation of the list of names in 
    lst separated by commas and "and." *)
let print_string_list lst = 
  let rec print_help = function
    |[] -> ""
    |h::t when t = [] -> "and " ^ Player.get_name h
    |h::t -> Player.get_name h ^ ", " ^ print_help t in 
  if List.length lst = 1 then Player.get_name (List.hd lst) else print_help lst

(* [table_wins lst] prints a string on the terminal that lets the players know 
   they have beat the dealer in this round. *)
let table_wins lst = ANSITerminal.(print_string [green] (
    "\nCongratulations! " ^ print_string_list lst ^ " beat the dealer!\n" )
  ) 

(** [tied lst] prints a string on the terminal that lets the players know
    that they have tied with the dealer on this round. *)
let tied lst = ANSITerminal.(print_string [yellow] (
    print_string_list lst ^ " tied the dealer!\n" )
  ) 

(** [losers lst] prints a string on the terminal that lets the players know
    that they have lost to the dealer this round. *)
let losers lst = ANSITerminal.(print_string [red] (
    print_string_list lst ^ " lost to the dealer!\n" )
  ) 

(** [play_dealer game dealer] is an updated version of [game] after [dealer]
    their turn. *)
let rec play_dealer game dealer =
  if (Player.compare21 dealer = 0 && Player.number_of_cards dealer = 2) then
    let () = print_endline dealers_cards;
      ANSITerminal.(print_string [cyan] (
          "(" ^ string_of_cards (Player.get_hand dealer) ^ ").\n"));
      print_cards (Player.get_hand dealer);
      ANSITerminal.(print_string [green] (dealer_blackjack)) 
    in game else
  if (Player.compare21 dealer = 0) then 
    let () = ANSITerminal.(
        print_string [green] (dealer_21)) in game else
    let () = print_endline dealers_cards
    in ANSITerminal.(print_string [cyan] (
        "(" ^ string_of_cards (Player.get_hand dealer) ^ ").\n"));
    print_cards (Player.get_hand dealer);
    let () = Unix.sleepf (0.75) in
    match Computer.casino_dealer dealer with
    |Hit -> let new_game = Blackjack.hit game dealer in
      let new_player = Blackjack.get_player 0 new_game in
      if Player.is_playing new_player then
        play_dealer new_game new_player
      else let new_card = List.hd (Player.get_hand new_player) 
        in let () = ANSITerminal.(print_string [red] (
            "\nThe dealer drew a " ^ (Deck.card new_card) ^ " and BUSTED!\n" )) 
        in new_game
    |Stay -> let () = print_endline(dealer_stay); in game
    |_ -> failwith "computer dealer error"

(** [force_change_table game] is [game] after switching tables. *)
let rec force_change_table game =
  let rec remove_players game players = 
    match players with
    |[] -> game
    |h::t -> remove_players (remove_player game h) t in

  let cannot_buyin buyin min max = 
    let players = players game in
    let table = Bet.init buyin min max in
    List.filter (
      fun p -> not (is_dealer p) &&
               get_wallet (Player.get_bet p) < get_wallet table) players,
    List.filter (fun p -> not (is_dealer p)) players          
  in

  let rec confirm (low_p,all_p) game min max =
    if low_p = [] then 
      let () = ANSITerminal.(print_string [green] successful_table_change) 
      in change_table min max game else if low_p = all_p 
    then let () = ANSITerminal.(print_string [red] impossible_table_change) 
      in force_change_table game 
    else let () = ANSITerminal.(print_string [yellow] (
        elimination_table_change ^ print_string_list low_p ^ "\n")) 
      in print_string "> ";
      match read_line () with
      |"y" -> change_table min max (remove_players game low_p)
      |"n" -> game
      |_ -> ANSITerminal.(print_string [red] not_y_or_n); 
        confirm (low_p,all_p) game min max in

  let () = ANSITerminal.(print_string [blue] table_options) in
  print_string "> ";
  match read_line () with
  |"quit" -> print_endline quit; exit 0
  |n -> begin match int_of_string n with
      |exception _ -> print_endline not_a_number; force_change_table game
      |n when n = 1 -> let cannot_buyin = cannot_buyin 10000 2000 100000 in 
        confirm cannot_buyin game 2000 100000
      |n when n = 2 -> let cannot_buyin = cannot_buyin 2000 500 10000 in 
        confirm cannot_buyin game 500 10000
      |n when n = 3 -> let cannot_buyin = cannot_buyin 400 100 2000 in 
        confirm cannot_buyin game 100 2000
      |n when n = 4 -> let cannot_buyin = cannot_buyin 5 5 100 in 
        confirm cannot_buyin game 5 100
      |_ -> ANSITerminal.(print_string [red] invalid_table); 
        force_change_table game end

(** [check_scores game] goes through all the players and checks if they 
    have won, tied, or lost and updates the game accordingly. *)
let rec check_scores game = 
  let () = Unix.sleepf (0.75) in
  let won, tie , lost = rank_players game in
  let _ =  
    if won <> [] then table_wins won else ();
    if tie <> [] then tied tie else ();
    if lost <> [] then losers lost else (); in
  let updated_wallets = update_wallets    game in
  let removed_temps   = clean_after_split updated_wallets in 
  let removed_players = remove_players    removed_temps in 
  if List.filter (fun p -> not(Player.is_cpu p)) 
      (Blackjack.players removed_players) |> List.length = 1 then 
    let () = ANSITerminal.(print_string [red] (everyone_out_of_chips));
    in removed_players else if need_to_switch removed_players then 
    let () =  print_endline must_switch_tables in 
    match read_line () with
    |"quit" -> print_endline quit; exit 0
    |"stay" -> remove_under_min removed_players
    |"leave" -> force_change_table removed_players
    |_ -> ANSITerminal.(print_string [red] not_stay_or_leave); check_scores game
  else removed_players  

(** [loop_through_players players] is a game state of blackjack after going 
    through each player in [players] and 
    giving everyone a turn. *)
let rec loop_through_players game = function
  | [] -> game
  | p::ps when Player.is_cpu p -> 
    let () = ANSITerminal.(print_string [blue] (
        "-------------------------------------------------" ^
        "\n\nIt is " ^ Player.get_name p ^ "'s turn now.\n")) in
    loop_through_players (play_cpu game (Player.get_id p)) ps
  | p::ps -> 
    if Player.is_dealer p then
      let () = ANSITerminal.(print_string [blue] (dealers_turn))  in
      play_dealer game p
    else 
      let () = ANSITerminal.(print_string [blue] (
          "-------------------------------------------------" ^
          "\n\n" ^  Player.get_name p ^ start_player_turn));
        print_string "> ";
      in
      match read_line () with
      |"quit" -> print_endline quit; exit 0
      |_ -> loop_through_players (play_turn game (Player.get_id p)) ps

(** [get_bets game players] takes in the amount each player in game [game]
    wants to bet for that round. If a player cannot make the minimum bet
    they are removed from the game. *)
let rec get_bets game = function
  |[] -> game
  |p::ps -> 
    if Player.is_cpu p 
    then
      (let cpu_bet = Computer.card_counter game p in 
       let cpu_bet_s = string_of_int cpu_bet in
       Unix.sleepf(1.25);
       let () = print_endline( 
           (get_name p) ^ " has chosen to bet: " ^ cpu_bet_s) in
       get_bets (Blackjack.bet game p cpu_bet) ps) 

    else if Player.is_dealer p 
    then get_bets game ps 
    else
      let min = Bet.get_min (Player.get_bet p) in
      let max = Bet.get_max (Player.get_bet p) in
      let wallet = Bet.get_wallet (Player.get_bet p) in
      print_endline("\n" ^ Player.get_name p ^ 
                    ", how much would you like to bet this round? " ^ 
                    "Or type \"leave\" to leave the table.");
      print_endline("Your wallet contains " ^
                    (string_of_int wallet) ^ " chips.");
      print_string  "> ";
      match read_line () with
      | "quit" -> print_endline quit; exit 0
      | "leave" -> ANSITerminal.(print_string [red] (leave_game));
        get_bets (Blackjack.remove_player game p) ps
      | n -> (match int_of_string n with
          |exception _ ->  
            ANSITerminal.(print_string [red] (bet_non_number));
            get_bets game (p::ps)
          |n when n < min  || n > max -> 
            ANSITerminal.(
              print_string [red] (bet_outside_table_max_min min max));
            get_bets game (p::ps)
          |n when n > wallet -> 
            ANSITerminal.(print_string [red] (bet_too_high));
            get_bets game (p::ps)
          |n -> get_bets (Blackjack.bet game p n) ps)

(** [change_tables game] is [game] with switched tables if a player chooses. *)
let rec change_tables game =
  let rec remove_players game players = 
    match players with
    |[] -> game
    |h::t -> remove_players (remove_player game h) t in

  let cannot_buyin buyin min max = 
    let players = players game in
    let table = Bet.init buyin min max in
    List.filter (
      fun p -> not (is_dealer p) &&
               get_wallet (Player.get_bet p) < get_wallet table) players,
    List.filter (fun p -> not (is_dealer p)) players          
  in

  let rec confirm (low_p,all_p) game min max =
    if low_p = [] then 
      let () = ANSITerminal.(print_string [green] successful_table_change) 
      in change_table min max game else if low_p = all_p 
    then let () = ANSITerminal.(print_string [red] impossible_table_change) 
      in change_tables game 
    else let () = ANSITerminal.(print_string [yellow] (
        elimination_table_change ^ print_string_list low_p ^ "\n")) 
      in print_string "> ";
      match read_line () with
      |"y" -> change_table min max (remove_players game low_p)
      |"n" -> game
      |_ -> ANSITerminal.(print_string [red] not_y_or_n); 
        confirm (low_p,all_p) game min max in

  let () = print_endline switch_table in
  print_string "> ";
  match read_line () with
  |"y" -> begin let () = ANSITerminal.(print_string [blue] table_options); 
                  print_string "> " in 
      match read_line () with
      |"quit" -> print_endline quit; exit 0
      |n -> match int_of_string n with
        |exception _ -> print_endline not_a_number; change_tables game
        |n when n = 1 -> let cannot_buyin = cannot_buyin 10000 2000 100000 in 
          confirm cannot_buyin game 2000 100000
        |n when n = 2 -> let cannot_buyin = cannot_buyin 2000 500 10000 in 
          confirm cannot_buyin game 500 10000
        |n when n = 3 -> let cannot_buyin = cannot_buyin 400 100 2000 in 
          confirm cannot_buyin game 100 2000
        |n when n = 4 -> let cannot_buyin = cannot_buyin 5 5 100 in 
          confirm cannot_buyin game 5 100
        |_ -> ANSITerminal.(print_string [red] invalid_table); 
          change_tables game
    end
  |"n" -> game
  |"quit" -> print_endline quit; exit 0
  |_ -> ANSITerminal.(print_string [red] not_y_or_n); 
    change_tables game

(** [get_json_list ()] is a list of all the json games. *)
let get_json_list unit = 
  let dir = Unix.opendir "." in

  let contains_json file = 
    try
      let i = String.rindex file '.' in
      String.sub file i (String.length file - i) = ".json"
    with _ -> false in

  let rec read_files dir acc =
    match Unix.readdir dir with
    |file when contains_json file -> 
      (try
         let game = String.sub file 0 (String.rindex file '.') in
         read_files dir (game::acc)
       with _ -> read_files dir acc )
    |file -> read_files dir acc 
    |exception End_of_file -> (Unix.closedir dir); acc in
  read_files dir []

(** [new_game_prompt g] allows users to to start a new round  *)
let rec new_game_prompt g = 
  if List.length (players g) = 1 then () else
    let () = print_endline play_again in
    print_string  "> "; 
    match read_line () with
    | "yes" -> 
      let change_table = change_tables g in
      let reset_g = reset change_table in
      let game_with_bets = get_bets reset_g (Blackjack.players reset_g) in
      let new_game = loop_through_players game_with_bets 
          (Blackjack.players game_with_bets) in
      let new_game' = check_scores new_game in 
      new_game_prompt new_game'
    | "no" -> begin
        let rec save_state_prompt unit =
          print_endline save_game_question;
          print_string  "> ";
          match read_line () with
          |"quit" -> print_endline quit; exit 0
          | "yes" -> begin
              print_endline save_game_name;
              print_string  "> ";
              match read_line () with 
              |s ->  
                let s = String.map (fun c -> if c = ' ' then '_' else c) s in
                if List.mem s (get_json_list ()) then begin
                  ANSITerminal.(print_string [red] overwrite_file);
                  print_string "> ";
                  match read_line () with
                  |"y" -> begin save_state g (s ^ ".json");  
                      print_endline("Your game was saved as " ^ s ^ ".json");
                      print_endline save_and_quit;
                      exit 0 end
                  |"n" -> save_state_prompt ()
                  |_ -> print_endline not_y_or_n; save_state_prompt () end
                else
                  save_state g (s ^ ".json");  
                print_endline("Your game was saved as " ^ s ^ ".json");
                print_endline save_and_quit;
                exit 0 end
          | "no" -> print_endline quit; exit 0 
          | _ -> 
            print_endline not_yes_or_no;
            save_state_prompt ();
        in 
        save_state_prompt ();
      end

    | "quit" -> print_endline quit; exit 0
    | _ -> print_endline not_yes_or_no; new_game_prompt g

(** [play_round game curr_player] starts a round for [player] in [game]. 
    Within a round, the player may request multiple or no cards. *)
let rec play_round game = 
  let game_with_bets = get_bets game (Blackjack.players game) in
  let new_game = loop_through_players game_with_bets 
      (Blackjack.players game_with_bets) in
  let g = check_scores new_game in 
  new_game_prompt g

(** [get_player_names i n acc] is a string list of the names of [n] players
    playing a game of blackjack. *)
let rec get_player_names i n acc =
  if i > n then acc else 
    let () = print_endline(
        "\nPlayer " ^ string_of_int(i) ^ " please enter your name.");
      print_string("> "); in
    match read_line () with
    |"quit" -> print_endline quit; exit 0
    |"" -> ANSITerminal.(print_string [red] no_name_entered); 
      get_player_names i n acc
    |name -> get_player_names (i+1) n (name::acc)

(** [get_json_game ()] is all the json games in the current directory. *)
let get_json_games unit = 
  let dir = Unix.opendir "." in

  let contains_json file = 
    try
      let i = String.rindex file '.' in
      String.sub file i (String.length file - i) = ".json"
    with _ -> false in

  let rec read_files dir acc =
    match Unix.readdir dir with
    |file when contains_json file -> 
      (try
         let game = String.sub file 0 (String.rindex file '.') in
         read_files dir (acc ^ game ^ "\n")
       with _ -> read_files dir acc )
    |file -> read_files dir acc 
    |exception End_of_file -> (Unix.closedir dir); acc in
  read_files dir "\nYour currently saved game files are:\n"

(** [create_computer_players n acc] is a list of the names of [n] 
    computer players. *)
let rec create_computer_players n acc =
  if n = 0 then acc else 
    create_computer_players (n-1) (("Computer " ^ (string_of_int n))::acc)

(** [multiplayer ()] is a multiplayer game of blackjack. *)
let rec multiplayer () = 
  print_endline old_or_new_game;

  let rec get_computer_numbers np =
    print_endline num_cpus;
    print_string "> ";
    match read_line () with
    | "quit" -> print_endline quit; exit 0
    | n -> begin match int_of_string n with 
        | exception _ -> ANSITerminal.(
            print_string [red] (not_a_number)); 
          get_computer_numbers np
        | n when n < 0 -> begin
            ANSITerminal.(print_string [red] (num_less_than_zero)); 
            get_computer_numbers np end
        | n when (n+np) > 7 -> 
          ANSITerminal.(print_string [red] (Strings.num_greater_than_seven)); 
          get_computer_numbers np 
        | n -> n
      end in

  let rec new_game unit = 
    print_endline num_users;
    print_string  "> "; 
    match read_line () with
    | "go back" -> multiplayer ()
    | "quit" -> print_endline quit; exit 0
    | number -> begin
        match int_of_string number with
        | exception _ -> ANSITerminal.(print_string [red] (not_a_number));
          new_game ()
        | n when n <= 0 || n > 7 -> 
          ANSITerminal.(print_string [red] (less_than_one_player));
          new_game ()
        | n -> 
          let c = create_computer_players (get_computer_numbers n) [] in 
          let p = get_player_names 1 n [] in
          play_round (Blackjack.init n p c)
      end in 

  let rec old_game unit = 
    print_endline open_old_game;
    print_string  "> ";
    match read_line () with 
    | "quit" -> print_endline quit; exit 0
    | "game list" -> print_endline (get_json_games ()); old_game();
    | "go back" -> multiplayer ()
    | file ->
      let file_name = file ^ ".json" in
      try 
        let g = load_state (Yojson.Basic.from_file file_name) in 
        print_endline load_game;
        play_round g
      with _ ->
        print_endline invalid_game_file;
        old_game (); in 

  match get_char () with
  | 'o' -> old_game ()
  | 'n' -> new_game ()
  | 'b' -> ()
  | 'g' -> print_endline (get_json_games ()); multiplayer ();
  | 'q' -> print_endline quit; exit 0
  |  _  -> multiplayer ()

(** [main ()] prompts for the game to play, then starts it. *)
let rec main unit = 
  ANSITerminal.(print_string [blue;Bold] (single_or_multi));
  print_newline ();
  match get_char () with
  |'1' -> Training.init (); main ()
  |'2' -> multiplayer (); main ()
  |'q' -> print_endline quit; exit 0
  |_ -> ANSITerminal.(print_string [red] not_one_or_two); main ()

(* Execute the game engine. *)
let () = 
  let () = ANSITerminal.resize 80 50 in
  print_endline(welcome_string); 
  main ()