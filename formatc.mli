(** [string_of_card_idx (x, y)] is a string lists of the card
    corresponding to each int [x] in range 1:13 (acoording to card value) and
    suit acoording to [y] in {Integers} .
    For [y] equal to 0 the suit is spades.
    For [y] equal to 1 the suit is hearts.
    For [y] equal to 2 the suit is diamonds.
    For any other [y] the suit is clubs.
    Each string list has six elements corresponding
    to each row in the ascii card art. 

    Example: 
    [string_of_card_idx (1,0)] is
    [" _____ " ;
     "|A    |" ;
     "|     |" ;
     "|  ♠  |" ;
     "|     |" ;
     "|____A|"]

*)
val string_of_card_idx : (int * int) -> string list