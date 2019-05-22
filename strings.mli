(** This module contains strings that need to be printed throughout the game *)

(** [single_or_multi] is the string printed asking a user whethere the want
    to play a single or multiplayer game. *)
val single_or_multi : string

(** [welcome_string] is the initial message the users see when they launch the
    program. *)
val welcome_string : string

(** [got_blackjack] is the string printed when a player gets blackjack. *)
val got_blackjack : string

(** [dealer_top_card] is string printed when a dealer is about to show their 
    top card. *)
val dealer_top_card : string

(** [load_game] is what is displayed when a game file is loading. *)
val load_game : string

(** [rules_and_info] is the string displayed when a user types 'help'. *)
val rules_and_info : string

(** [get_score] is the string printed when a user enters the command score. *)
val get_score : string

(** [dealers_turn] is the string printed to notify the players that it is
    the dealer's turn. *)
val dealers_turn : string

(** [dealer_stay] is the string printed to notify the players that the 
    dealer has chosen to stay. *)
val dealer_stay : string

(** [dealers_cards] is the string printed to notify the players of the dealers
    cards when it is the dealers turn. *)
val dealers_cards : string

(** [dealer_21] is the string printed to notify the players that the dealer
    has gotten 21. *)
val dealer_21 : string

(** [dealer_blackjack] is the string printed to notify the players that the 
    dealer has gotten blackjack *)
val dealer_blackjack : string

(** [successful_table_change] is the string printed when a successful table
    change occurs. *)
val successful_table_change : string

(** [impossible_table_change] is the string printed when a user tries to 
    switch to a table where no player can pay for the buyin. *)
val impossible_table_change : string

(** [elimination_table_change] is the string printed when a user tries to 
    switch to a table where one or more player cannot make the buyin. *)
val elimination_table_change : string

(** [make_move] is the string printed asking a player to make a move. *)
val make_move : string

(** [type_c] is the string printed when a user needs to type "c" to
    continue the game. *)
val type_c : string

(** [open_old_game] is the string printed when a user chooses to play an
    existing game. *)
val open_old_game : string

(** [num_users] is the string printed when asking the user for the number
    of human players. *)
val num_users : string

(** [num_cpus] is the string printed to ask the user how many computer
    players they want to play with. *)
val num_cpus : string

(** [old_or_new_game] is the string printed when a user is asked if they
    want to start a new or old game. *)
val old_or_new_game : string

(** [save_game_name] is the string printed to ask the user what they want to
    name their saved game. *)
val save_game_name : string

(** [play_again] is the string printed asking a user whether they would like
    to play another round. *)
val play_again : string

(** [save_game_question] is the string printed asking a player whether they
    would like to save there game. *)
val save_game_question : string

(** [start_player_turn] is the string to start a player's turn. *)
val start_player_turn : string

(** [switch_table] is the string printed at the end of a round asking a user
    if they would like to switch tables for the next round. *)
val switch_table : string

(** [table_options] is the string printed when a user wants to switch tables. *)
val table_options : string

(** [must_switch_tables] is the string printed when the table must be changed
    or certain player will be removed from the game. *)
val must_switch_tables : string

(** [overwrite_file] is the string printed when a user tries to save a game 
    file with an already existing file name. *)
val overwrite_file : string

(** [quit] is what is printed when a user quits the game. *)
val quit : string

(** [save_and_quit] is the string printed when a user saves their game. *)
val save_and_quit : string

(** [everyone_out_of_chips] is the string printed when everyone runs out of
    chips. *)
val everyone_out_of_chips : string

(** [leave_game] is the string printed when a single player chooses to
    leave the game. *)
val leave_game : string

(** [invalid_game_file] is the string printed when a user enters an 
    invalid game file. *)
val invalid_game_file : string

(** [less_than_one_player] is the string printed when a user tries to start
    a game with less than one human playing. *)
val less_than_one_player : string

(** [not_a_number] is the string printed when a user does not enter a number 
    when prompted to input a number. *)
val not_a_number : string

(** [num_less_than_zero] is the string printed when a user enters a number
    less than zero for the number of computer pllayers.  *)
val num_less_than_zero : string

(** [num_greater_than_seven] is the string printed when a user enters a number
    for computer pllayers that makes the total count of players at 
    a table greater than 7.  *)
val num_greater_than_seven : string

(** [no_name_entered] is the string printed when a player does not enter
    anything when prompted for their name. *)
val no_name_entered : string

(** [not_yes_or_no] is the string printed when a user does not enter
    ["yes"] or ["no"] when prompted for a yes or no. *)
val not_yes_or_no : string

(** [not_y_or_n] is the string printed when a user does not enter
    ["y"] or ["n"] when prompted for a y or n. *)
val not_y_or_n : string

(** [bet_too_high] is the string printed when a user tries to bet more chips 
    than they have in their wallet. *)
val bet_too_high : string

(** [bet_outside_table_max_min min max] is the string printed when a user 
    tries to make a bet outside of the table [min] and [max] values. *)
val bet_outside_table_max_min : int -> int -> string

(** [bet_non_number] is the string printed when a user does not enter a 
    number for their bet. *)
val bet_non_number : string

(** [bad_command] is the string printed when a player enters a bad command
    during their turn. *)
val bad_command : string

(** [invalid_split] is the string printed when a player tries to split but
    they do not have a valid hand to split. *)
val invalid_split : string

(** [invalid_save] is the string printed when a player tries to save in the
    middle of a round. *)
val invalid_save : string

(** [invalid_load] is the string printed when a player tries to load in the
    middle of a round. *)
val invalid_load : string

(** [invalid_table] is the string printed when a user inputs an invalid table
    when attempting to switch tables. *)
val invalid_table : string

(** [not_stay_or_leave] is the string printed when a user doesn't enter
    stay or leave when prompted to. *)
val not_stay_or_leave : string

(** [not_one_or_two] is the string printed when a played does not enter 1 or 2
    when prompted to do so. *)
val not_one_or_two : string

(** [welcome_m] is the string printed when a player enters training mode. *)
val welcome_m : string

(** [quit_m] is the string printed when a user quits in training mode. *)
val quit_m : string

(** [not_command_m] is the string printed when a user enters an invalid 
    command in training mode. *)
val not_command_m : string

(** [begin_learning_mode_m] is the string printed when a user begins learning 
    mode. *)
val begin_learning_mode_m : string

(** [begin_guided_mode_m] is the string printed when a user begins guided 
    mode. *)
val begin_guided_mode_m : string

(** [guided_mode_turn_m name] is the string printed when [name] starts their
    turn in guided mode. *)
val guided_mode_turn_m : string -> string

(** [yourself_mode_turn_m name] is the string printed when [name] starts their
    turn in try it yourself mode. *)
val yourself_mode_turn_m : string -> string

(** [guided_mode_end_of_round] is the string printed at the end of a round in
    guided mode. *)
val guided_mode_end_of_round : string

(** [yourself_mode_end_of_round] is the string printed at the end of a round in
    try it yourself mode. *)
val yourself_mode_end_of_round : string

(** [p_stats game p] is the probability of [p] busting in [game] and [p]'s
    overall advantage over the dealer. *)
val p_stats : Blackjack.t -> Player.t -> string

(** [make_move_guided] is the string printed when a user is about to make a
    move in guided mode. *)
val make_move_guided : string

(** [make_move_yourself] is the string printed when a user is about to make a
    move in yourself mode. *)
val make_move_yourself : string

(** [guided_mode_hit move] is the string printed to tell the user that the
    computer would make move [move]. *)
val guided_mode_hit : Command.command -> string

(** [correct_move] is the string printed when a user makes the same move 
    that the computer ai would make. *)
val correct_move : string

(** [incorrect_move c] is the string printed when a user does not 
    make move [c]. *)
val incorrect_move : Command.command -> string

(** [not_ai_move] is the string printed when a user attempts to make a 
    move different than the computer ai would make. *)
val not_ai_move : string 

(** [not_c_or_d] is the string printed when a user does not enter c or d
    when prompted to do so. *)
val not_c_or_d : string

(** [start_dealer_turn_m] is the string printed to start the dealer's turn in
    training mode. *)
val start_dealer_turn_m : string

(** [leave_guided_mode] is the string printed when a user leaves while
    playing guided mode. *)
val leave_guided_mode : string

(** [leave_yourself_mode] is the string printed when a user leaves while
    playing try it yourself mode. *)
val leave_yourself_mode : string

(** [beat_dealer] is the string printed when a player beats the dealer in
    training mode. *)
val beat_dealer : string

(** [tied_dealer] is the string printed when a user ties the dealer in
    training mode. *)
val tied_dealer : string

(** [lost_to_dealer] is the string printed when a user loses to the dealer
    in training mode. *)
val lost_to_dealer : string

(** [card_count_m g] is the string printed to inform the uder of the current
    card count in game [g]. *)
val card_count_m : Blackjack.t -> string

(** [get_bet_m p wallet] is the string printed asking player [p] to bet while
    in training mode when [p] have a wallet of size [wallet]. *)
val get_bet_m : Player.t -> int -> string

(** [out_of_chips] is the string printed when a player runs out of chips
    while playing training mode. *)
val out_of_chips : string

(** [begin_yourself_mode_m] is the string printed when a user begins try it
    yourself mode. *)
val begin_yourself_mode_m : string

(** [get_name_m] is the string printed asking a user for their name in 
    training mode. *)
val get_name_m : string

(** [no_name_m] is the string printed when a user does not enter a name in
    training mode when prompted to do so. *)
val no_name_m : string

(** [place_bet_m name i] is the string printed when a user with name [name]
    is about to bet when the card count is [i]. *)
val place_bet_m : string -> string

(** [bet_buy_in_m b] is the string printed telling the user to be the buyin [b]
    while playing in training mode. *)
val bet_buy_in_m : int -> string

(** [wallet_m w] is the string telling a user in trainng that their wallet is 
    of size [w]. *)
val wallet_m : int -> string

(** [play_again_m] is the string printed prompting a user to play again in
    traing mode. *)
val play_again_m : string

(** [header_m name] is the string prompting a user with name [name] that it
    is their turn. *)
val header_m : string -> string

(** [p_advantage_m adv] is the string telling a user that their advantage over
    the dealer is [adv]. *)
val p_advantage_m : string -> string

(** [bet_m i] is the string suggesting that a user bets [i] chips. *)
val bet_m : int -> string

(** [guided_mode_m] is the string saying guided mode. *)
val guided_mode_m : string

(** [yourself_mode_m] is the string saying try it yourself mode. *)
val yourself_mode_m : string

(** [learning_mode_m] is the header for learning mode. *)
val learning_mode_m : string

(** [bet_m cc i] is the string telling the user how much they should bet. *)
val bet_m : int -> int -> string

(** [tutorial_help_m] is the string with the available commands. *)
val tutorial_help_m : string

(* [p_advantage_m s] is the string with the player's advantage over 
   the dealer. *)
val p_advantage_m : string -> string

(* [cc_m cc] is the string with the card count for previous round. *)
val cc_m : int -> string

(* [c_value_m add] is the string with the card count for one card. *)
val c_value_m : int -> string

(* [cc_m cc] is the string with the updated card count. *)
val cur_cc : int -> string

(* [score m] is the string with the score of the player. *)
val score_m: int -> string

(* [p_bust_m p] is the string with the probability of the player going bust. *)
val p_bust_m : Player.t -> string

(* [sd_move_m sd] is the string with the suggested move. *)
val sd_move_m : string -> string

(* [make_move_m] is the string that asks the user for a move. *)
val make_move_m : string

(* [new_round_m] is the header for a New Round. *)
val new_round_m : string

(** [leave_yourself_m] is the string printed when a user leaves try it
    yourself mode. *)
val leave_yourself_m : string

(** [bad_bet_m i] is the string printed when a user inputs a bet different from
    [i] in try it yourself mode. *)
val bad_bet_m : int -> string

(** [no_hints_allowed] is the string printed when a user asks for a hint in
    yourself mode. *)
val no_hints_allowed : string
