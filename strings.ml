open Computer
open Command

(* display info to users *)
let single_or_multi =
  "\nWould you like to do training mode (1) or play an actual game of 
Blackjack (2)? (Type 'q' to quit)

1. Training Mode  (single player)
2. Blackjack Game (multiplayer)\n"

let welcome_string = 
  "\n\n                Welcome to Blackjack. 

  In this text based version of blackjack each player 
will be competing against the dealer. At the beginning of each round you will 
be dealt two cards and when it is your turn you will have the option to hit,
stay, double your bet, or split your hand. The objective is to get as close 
to 21 as possible without going over. If you go over 21 you bust and 
automatically lose that round. 

  Each player will start with 100 chips when they first start playing and each
round there will be a minimum bet of 5 chips and a maximum bet of 100 chips.
At the start of the round if you do not have enough chips to pay the minimum
bet you will be removed from the table. 

  As you gain more chips you will have the option to switch to higher stakes 
tables with higher minimum and maximum bets. At the end of each round you
will be given the option to switch to different tables. If your chip count 
runs below the minimum bet at a higher buyin table, you will have the option 
to buyin at a lower stakes table.

  Each table can hold a maximum of 8 people (including the dealer) so you may 
add up to 7 seven players to the table. These seven players can include human 
and computer players but must contain at least one human player. Beware though,
the computer players have been trained to always make the smartest move.

  Additionally, at the end of a round you will have the option to save your game
if you would like to continue your game at another time. After you save, the
next time you play you will be able to continue where you left off by start 
an old game and entering the name of your saved game.

    The card values are the same as standard blackjack card values:\n
    Jack, Queen, King : 10
    2 - 10 : values of the number of the card
    Ace : 1 or 11 (whichever gets you closer to 21)

    You also have the option to play a single player training mode which will
    allow you practice Blackjack and get feedback after each move you make.\n"

let got_blackjack = "Congratulations You Got Blackjack!\n"

let dealer_top_card = "The dealer's top card is:"

let load_game = "Loading game... \n\n"

let rules_and_info = 
  "The commands are: \"help\", \"hit\", \"stay\", \"double\", \"split\" and" ^ 
  " \"score\".\nThe goal of blackjack is to have the sum of your cards be " ^ 
  "as close to 21 as possible. The commands are as follows:\n\n" ^
  " 'hit' draws another card, and if your new sum exceeds 21, then you've " ^
  "gone 'bust' aka you lose.\n 'stay' refers to when you don't want another " ^ 
  "card.\n 'double' doubles your current bet and you agree to stay " ^ 
  "after your next card.\n 'split' is possible when you have two cards " ^ 
  "of the same rank: you 'split' your 2 cards into two separate hands " ^ 
  "and play on each.\n 'score' gives you your current score." ^ 
  "\n\n type 'c' and press 'enter' to continue \n"

let get_score = "Your score is: "

let dealers_turn = "-------------------------------------------------" ^
                   "\n\nIt is the dealer's turn now."

let dealer_stay = "\nThe dealer has chosen to stay.\n"

let dealers_cards = "\nThe dealer's cards are: "

let dealer_21 = "The Dealer Got 21!\n"

let dealer_blackjack = "The Dealer Got Blackjack!\n"

let successful_table_change = 
  "\nAll players have been successfully moved to the new table.\n"

let impossible_table_change =
  "\nNo player can pay for the buyin at this table. Cannot switch" ^ 
  " to this table.\n"

let elimination_table_change = 
  "\nAre you sure you would like to switch tables? (y/n)\nThe " ^ 
  "following players will be eliminated from the game because they " ^ 
  "cannot pay for the buyin:\n"

(* request info from users *)
let make_move = "Would you like to hit (h), stay (s), double (d), or " ^  
                "split (sp)?\n" ^
                "Or type \"leave\" to leave the table.\n> "

let type_c = "Please type 'c' to continue"

let open_old_game = 
  "\nEnter the name of the saved game or type \"go back\" to go back or type " ^
  "\"game list\" for a list of available games."

let num_users = "\nPlease enter the number of human " ^ 
                "players or type \"go back\" to go back:"

let num_cpus = "\nHow many computer players would you like in the game? " ^ 
               "(Type \"0\" for no computers) "

let old_or_new_game = 
  "\nWould you like to continue an old game or start a new one? (o/n)\n" ^
  "Or type \"b\" to go back."

let save_game_name = "Enter the name of the game you would like to save:"

let play_again = "Would you like to play again? (yes/no)"

let save_game_question = "Would you like to save your game? (yes/no)"

let start_player_turn = ", it is your turn now. Press enter " ^
                        "when you are ready to play your turn.\n"

let switch_table = "\nWould you like to switch to a different table? (y/n)"

let table_options = 
  "\nEnter the number of the table you would like to play at: \n\n" ^
  "1. High Rollers - buyin: 10000, min bet: 2000, max bet: 100000\n" ^
  "2. Medium Rollers - buyin: 2000, min bet: 500, max bet: 10000\n" ^
  "3. Low Rollers - buyin: 400, min bet: 100, max bet: 2000\n" ^ 
  "4. Just Starting - buyin: 5, min bet: 5, max bet: 100\n"

let must_switch_tables =
  "Not everyone at your table can pay the minimum bet at this table.\n" ^
  "You must now either switch tables or stay at this table and all " ^ 
  "players who cannot pay the minimum bet will be removed from the game.\n" ^
  "Please enter \"stay\" or \"leave\".\n"

let overwrite_file =
  "\nThis file already exists. Saving this game file will overwrite the " ^
  "existing game file with this name.\nAre you sure you would like to " ^
  "save your game with this name? (y/n)\n"

(* user quitting *)
let quit = "\nGoodbye! Thanks for Playing!"

let save_and_quit = "See ya next time!"

let everyone_out_of_chips = 
  "\nSorry, every human player in the game has run out of chips/left. " ^ 
  "You now can start a new game or play training mode.\n" 

let leave_game = "\nYou are out of the game.\n\n"

(* user errors *)
let invalid_game_file = "\nI couln't find that file. Try typing " ^
                        "\"game list\" for a list of games\n"

let less_than_one_player = 
  "\nCannot start a game with less than 1 player" ^ 
  " or more than 7 players per table.\n"

let not_a_number = "\nPlease enter a number.\n"

let num_less_than_zero = 
  "\nYou can't have less than 0 computers.\n"

let num_greater_than_seven = 
  "\nThis table only allows up to 7 players total.\n" 

let no_name_entered = "\nPlease enter a name.\n"

let not_yes_or_no = "I only understand \"yes\" or \"no.\""

let not_y_or_n = "\nPlease enter \"y\" or \"n\".\n"

let bet_too_high = "\nYou can not bet more than " ^ 
                   "the number of chips in your wallet\n"

let bet_outside_table_max_min min max = 
  "\nPlease enter a number within the " ^ 
  "min and max bets for the table (" ^ 
  string_of_int min ^ " - " ^ string_of_int max ^ ")\n"

let bet_non_number = "\nPlease enter a number for the amount" ^ 
                     " you would like to bet.\n"

let bad_command = 
  "\nSorry, I don't understand that command.\n" ^
  "Here is a list of available commands:\n" ^
  "hit, stay, score, quit, double, split, help\n" ^
  "Type \"help\" for more info on the commands.\n"

let invalid_split = "To split, you must have 2 cards of the same rank.\n"

let invalid_save = "You must wait until the round is over to Save."

let invalid_load = "You can only Load at the beginning of the round."

let invalid_table = "\nPlease enter a valid table number\n"

let not_stay_or_leave = "\nPlease enter \"stay\" or \"leave\".\n"

let not_one_or_two = "\nPlease enter \"1\" or \"2\".\n"

(* Training strings *)
let welcome_m = 
  "\n\n  Welcome to training mode! Here you will have the opportunity to play 3 
different training modes, each of a different difficulty.\n
 • Learning mode will walk you through the best strategy for blackjack.\n
 • Guided mode will allow you to play on your own but will give you hints if
   you ask for them.\n
 • Try it yourself will put your knowledge to the test by allowing you to play
   a round on your own and tell you if the moves you made were the best choice.
\nWhich game mode would you like to play: \n\n" ^
  "\nl - learning mode\ng - guided mode\n" ^
  "y - try it yourself\ne - exit training mode\nq - quit\n"

let quit_m = "\nAre you sure you want to quit? 
no  - n
yes - y / enter"

let not_command_m = "\nNot a valid command... try again.\n"

let begin_learning_mode_m = 
  "\nYou are now in learning mode:
You will be guided through how to play a perfect game and get the best
odds! At the end of this interactive experience you will be a pro at
blackjack!"

let begin_guided_mode_m =
  "\nWelcome to guided mode.\nHere you will play through a game of
blackjack with the dealer and at any time you will be able to type 
\"hint\" to get a hint as to what your best move is.\n
Additionally, before each bet you place we will give you the card count 
but will not help you with betting as that is covered in learning mode.\n"

let guided_mode_turn_m name =
  "\n" ^ name ^ ", you are about to start a turn in guided mode.\n" ^ 
  "Remember at any time you can type 't' to get a hint.\n " 

let yourself_mode_turn_m name =
  "\n" ^ name ^ ", you are about to start a turn in try it yourself mode.\n" ^ 
  "Remember hints are allowed in this mode.\n "

let guided_mode_end_of_round =
  "\nWould you like to:\n" ^ 
  "p: play another round of guided mode\n" ^ 
  "s: switch to a different training mode\n" 

let yourself_mode_end_of_round =
  "\nWould you like to:\n" ^ 
  "p: play another round of yourself mode\n" ^ 
  "s: switch to a different training mode\n" 

let p_stats game p = 
  "\nThe probablity of you busting if you hit is: " ^ 
  string_of_float (p_bust p) ^ "%\n" ^
  "Your advantage over the dealer is: " ^ 
  string_of_float (p_advantage game) ^ "%\n"

let make_move_guided = 
  "Would you like to hit (h), stay (s), double (d), or split (sp)?\n" ^
  "Type \"leave\" to leave the table or type \"hint\" for a hint.\n> "

let make_move_yourself = 
  "Would you like to hit (h), stay (s), double (d), or split (sp)?\n" ^ 
  "Or type \"leave\" to leave the table.\n> "

let guided_mode_hit move = 
  "\nThe computer would " ^ (command_to_string move) ^ " in this situation.\n"

let correct_move = 
  "\nGood job! You made the correct move!\n"

let incorrect_move c =
  "\nSorry! Your did not make the right move.\n" ^
  "The computer would " ^ (command_to_string c) ^ " in this situation.\n\n"

let not_ai_move =
  "\nYou did not make the same move as the computer ai.\nWould you like to " ^
  "continue with your move or make a different move?\n" ^
  "\"c\" : continue with your move\n" ^
  "\"d\" : make a different move"

let not_c_or_d = "\nPlease enter \"c\" or \"d\".\n"

let start_dealer_turn_m = 
  "\nThe dealer's turn is about to start. " ^ 
  "Hit \"enter\" to continue.\n" ^ "> "

let leave_guided_mode =
  "\nYou have left guided mode.\n"

let leave_yourself_mode =
  "\nYou have left try it yourself mode.\n"

let beat_dealer =
  "\nCongratulations! You beat the dealer!\n"

let tied_dealer =
  "\nYou tied the dealer!\n"

let lost_to_dealer =
  "\nSorry! You lost to the dealer!\n"

let card_count_m g =
  "\nThe current card count is: " ^ string_of_int (Computer.update_cc g) ^ 
  "\nRemember: higher card counts have a higher probability of you winning.\n"

let get_bet_m p wallet =
  "\n" ^ Player.get_name p ^ 
  ", how much would you like to bet this round? " ^ 
  "Or type \"leave\" to leave this game mode.\n" ^
  "Your wallet contains " ^ (string_of_int wallet) ^ " chips.\n> "

let out_of_chips =
  "\nYou have run out of chips. " ^
  "You can restart this training mode or start a new one.\n"

let begin_yourself_mode_m = 
  "\nWelcome to try it yourself mode. In this mode you will be on your own.\n" ^
  "No hints will be given and the card count will not be given to you.\n" ^
  "However, you will still receive feedback after each move.\n" ^ 
  "This game mode is the most like a real blackjack gamee. Good Luck!\n"

let get_name_m = "Please enter your name: \n> "

let no_name_m = "Not a valid name\n"

let place_bet_m name = 
  "\nWelcome to the table, " ^ name ^ ".
\nThe first step is to place a bet, we suggest starting 
low when you fisrt join a table.
Over the next few rounds, your bet will change
depending on the \"card count\". 
For this tutorial, we will be using 
a sophisticated card count we called the Wong Method. 
\nRight now, the card count is: 0" ^
  "\nThe following values are used in the Wong System:

           Aces are tens are worth -2.
                   Nines are worth -1.
       Deuces and Sevens are worth +1.
Threes, Fours, and Sixes are worth +2.
                   Fives are worth +3.

"

let bet_buy_in_m b = 
  "At the beggining of the round, you should bet the buy in for the table: " 
  ^ (string_of_int b)

let wallet_m w = 
  "\nYour wallet contains " ^ (string_of_int w) ^ " chips.
Place your Bet:"

let play_again_m = 
  "\nPress 'e' to exit learning mode, 'q' to quit, 
or any other key to play again.\n"

let header_m name = 
  "_____________________________________________________
"^name^" , it is now your turn.

Remember:            Aces are tens are worth -2.
                             Nines are worth -1.
                 Deuces and Sevens are worth +1.
          Threes, Fours, and Sixes are worth +2.
                             Fives are worth +3.


"

let p_advantage_m adv = "Your current advantage over the dealer is :" ^ adv

let bet_m i = "this is how much you should bet: " ^ string_of_int i

let guided_mode_m = "\nGUIDED MODE\n"

let yourself_mode_m = "\nTRY IT YOURSELF MODE\n"

let leave_yourself_m = "You are no exiting try-it-yourself mode. Goodbye"

let bad_bet_m i = "Whoops! The correct bet given your current advantage is: " 
                  ^ string_of_int i 

let learning_mode_m = "\n\nLEARNING MODE\n"

let bet_m cc i = "\n\nBased on Card Count of " ^ string_of_int cc ^
                 ", you should bet " ^ string_of_int i ^ " coins.\n"

let tutorial_help_m = 
  "\nh - hit\ns - stay\nd - double\nsp - split\nleave - " ^ 
  "exit current training mode\nq - quit\n"

let p_advantage_m adv = 
  "Advantage over dealer : " ^ adv ^ "& \n"

let cc_m cc = 
  "      Prev Card Count : " ^ string_of_int cc ^ " \n"
let c_value_m add = 
  "       New Card Value : " ^ string_of_int add ^ " \n"

let cur_cc sum = 
  "   Current Card Count : " ^ string_of_int sum ^ " \n"
let score_m s = 
  "                Score : " 
  ^ string_of_int s ^ " \n\n"

let p_bust_m p = "The probablitily that you will bust if you hit is: " 
                 ^ string_of_float (p_bust p) ^ "0%. \n"

let sd_move_m sd = "The suggested move, based on your curent hand is: " 
                   ^ sd ^ " \n\n"
let make_move_m = "Would you like to hit (h), stay(s), double(d), or split(sp)?
> "

let new_round_m = "New Round! -------------------------------------------------"


let no_hints_allowed =
  "\nSorry. Hints are not allowed in this mode.\n"
