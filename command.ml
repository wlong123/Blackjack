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

exception Empty

exception Malformed

let parse str =
  let lst = str 
            |> String.trim 
            |> String.split_on_char ' ' 
            |> List.filter (fun x -> x <> "") in 
  match lst with
  | [] -> raise Empty
  | h::t -> 
    if h = "hit" || h = "h" then 
      (if t = [] then Hit else raise Malformed)
    else if h = "stay" || h = "s" then 
      (if t = [] then Stay else raise Malformed)
    else if h = "score" || h = "n" then 
      (if t = [] then Score else raise Malformed)
    else if h = "quit" || h = "q" then 
      (if t = [] then Quit else raise Malformed)
    else if h = "double" || h = "d" then 
      (if t = [] then Double else raise Malformed)
    else if h = "split" || h = "sp" then 
      (if t = [] then Split else raise Malformed)
    else if h = "load" || h = "l" then 
      (if t = [] then Load else raise Malformed)
    else if h = "save" || h = "sv" then 
      (if t = [] then Save else raise Malformed)
    else if h = "help"  then
      (if t = [] then Help else raise Malformed)
    else if h = "leave" then 
      (if t = [] then Leave else raise Malformed)
    else raise Malformed

let command_to_string = function
  | Help -> "help"
  | Hit -> "hit"
  | Stay -> "stay"
  | Score -> "score"
  | Quit -> "quit"
  | Double -> "double"
  | Split -> "split"
  | Load -> "load"
  | Save -> "save"
  | Leave -> "leave"

