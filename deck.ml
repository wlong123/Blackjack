type suit = Club | Spade | Heart | Diamond
type rank = Number of int | Jack | Queen | King | Ace of int

exception Empty

type card = suit * rank
type t = card list

let make () = 
  let rec make_help i =
    if i = 1 then [] else
      match i with
      | 14 -> (Club,Ace 11)::(Spade,Ace 11)::(Heart,Ace 11)::(Diamond,Ace 11)::
              make_help (i-1) 
      | 13 -> (Club,King)::(Spade,King)::(Heart,King)::(Diamond,King)::
              make_help (i-1) 
      | 12 -> (Club,Queen)::(Spade,Queen)::(Heart,Queen)::(Diamond,Queen)::
              make_help (i-1) 
      | 11 -> (Club,Jack)::(Spade,Jack)::(Heart,Jack)::(Diamond,Jack)::
              make_help (i-1) 
      | n -> (Club, Number n)::(Spade, Number n)::(Heart, Number n)::
             (Diamond, Number n)::make_help (i-1)
  in make_help 14

let shuffle deck = QCheck.Gen.(generate1 (shuffle_l deck))

let size deck = List.length deck

let draw = function
  | [] -> raise Empty
  | h::t -> (h, t)

let suit c = fst c

let rank c = snd c

let rec score acc = function
  | [] -> acc
  | h::t -> (match snd h with
      | Number n -> score (n+acc) t
      | Jack | Queen | King -> score (10+acc) t
      | Ace n -> score (n+acc) t)

let rec downgrade_ace = function
  | [] -> []
  | (s,Ace 11)::t -> (s,Ace 1)::t
  | h::t -> h::(downgrade_ace t)

let card c =
  let get_suit c =
    match suit c with
    | Club -> "Club"
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond" in
  let get_number c =
    match rank c with
    | Number n -> string_of_int n
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | Ace _ -> "Ace" in
  (get_number c) ^ " of " ^ (get_suit c)

let index_of_card c =
  let get_suit_idx c =
    match suit c with
    | Spade -> 0
    | Heart -> 1
    | Diamond -> 2 
    | Club -> 3 in
  let get_number_idx c =
    match rank c with
    | Ace _ -> 1 
    | Number n -> n 
    | Jack -> 11
    | Queen -> 12
    | King -> 13 in
  (get_number_idx c), (get_suit_idx c)

let number_of_card (c:card) = 
  match rank c with
  | King | Queen | Jack -> Number 10
  | n -> n

let mem c d = 
  List.mem c d

let rec filter acc f = function
  | [] -> acc
  | h::t when f h = true -> filter (h::acc) f t
  | h::t -> filter acc f t

let rec map f lst = 
  let rec map_h acc f = function 
    | [] -> acc
    | h::t -> map_h ((f h)::acc) f t in
  map_h [] f lst