(** [symbol int] is a string representing the suit of a card.
    For [int] equal to 0 the suit is spades.
    For [int] equal to 1 the suit is hearts.
    For [int] equal to 2 the suit is diamonds.
    For any other [int] the suit is clubs. *)
let symbol = function
  | 0 -> "♠"
  | 1 -> "♥"
  | 2 -> "♦"
  | _ -> "♣"

(** [row0] is the string "|     |" *)
let row0   = "|     |" 

(** [row1 s] is the string "|  [s]  |" *)
let row1 s = "|  " ^ s ^ "  |" 

(** [row2 s] is the string "| [s] [s] |" *)
let row2 s = "| " ^ s ^ " " ^ s ^ " |" 

(** [row3 s] is the string "|[s] [s] [s]|" *)
let row3 s = "|" ^ s ^ " " ^ s ^ " " ^ s ^ "|" 

(** [rank s] 
    The following funcitons are all string lists of the card
    by which they are named. Each string list has six elements corresponding
    to each row in the ascii card art. 
    Each card has string [s] as the suit representation. *)

let ace s = 
  [" _____ " ;
   "|A    |" ;
   row0   ;
   row1 s ;
   row0   ;
   "|____A|"]

let two s =
  [" _____ " ;
   "|2    |" ;
   row1 s ;
   "|     |" ;
   row1 s ;
   "|____2|"]

let three s = 
  [" _____ " ;
   "|3    |" ;
   row1 s ;
   row1 s ;
   row1 s ;
   "|____3|"]

let four s = 
  [" _____ " ;
   "|4    |" ;
   row2 s ;
   row0   ;
   row2 s ;
   "|____4|"]

let five s = 
  [" _____ " ;
   "|5    |" ;
   row2 s ;
   row1 s ;
   row2 s ;
   "|____5|"]

let six s = 
  [" _____ " ;
   "|6    |" ;
   row2 s ;
   row2 s ;
   row2 s ;
   "|____6|"]

let seven s = 
  [" _____ " ;
   "|7    |" ;
   row2 s ;
   row3 s ;
   row2 s ;
   "|____7|"]

let eight s = 
  [" _____ " ;
   "|8    |" ;
   row3 s ;
   row2 s ;
   row3 s ;
   "|____8|"]

let nine s = 
  [" _____ " ;
   "|9    |" ;
   row3 s ;
   row3 s ;
   row3 s ;
   "|____9|"]

let ten s = 
  [" _____ " ;
   "|10 "^s^" |" ;
   row3 s ;
   row3 s ;
   row3 s ;
   "|___10|"]

let jack s = 
  [" _____ " ;
   "|J    |" ;
   row0   ;
   row1 s ;
   row0   ;
   "|____J|"]

let queen s =
  [" _____ " ;
   "|Q    |" ;
   row0   ;
   row1 s ;
   row0   ;
   "|____Q|"]

let king s = 
  [" _____ ";
   "|K    |";
   row0   ;
   row1 s ;
   row0   ;
   "|____K|"]

let string_of_card_idx idx = 

  let get_num_fn r = begin
    match r with
    | 1 -> ace
    | 11 -> jack
    | 12 -> queen
    | 13 -> king
    | 2 -> two
    | 3 -> three
    | 4 -> four
    | 5 -> five
    | 6 -> six
    | 7 -> seven
    | 8 -> eight
    | 9 -> nine
    | 10 -> ten
    | _ -> failwith "Number out of card range in print card (ascii_art)"
  end in

  get_num_fn (fst idx) (symbol (snd idx))