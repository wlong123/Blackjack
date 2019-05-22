(**
   Parsing of player commands.
*)

(** The type of values representing a command. *)
type command = 
  | Help
  | Hit 
  | Stay 
  | Score 
  | Quit 
  | Double 
  | Split
  | Load
  | Save
  | Leave

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command]. For example, 
    [parse "    hit " ] returns command Hit. *)
val parse : string -> command

(** [command_to_string c] is a string representation of [c]. *)
val command_to_string : command -> string
