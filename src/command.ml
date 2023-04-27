type object_tuple = string * int

type command =
  | Request of object_tuple
  | Quit

exception Unrecognized
exception Empty

let rec parse_helper str =
  let split = String.split_on_char ' ' str in
  let filtered = List.filter (fun item -> item <> "") split in
  match filtered with
  | [] -> raise Empty
  | [ h; t1; t2 ] when h = "request" && List.length filtered > 2 ->
      Request (t1, int_of_string t2)
  | [ h ] when h = "quit" -> Quit
  | _ -> raise Unrecognized

let parse str = parse_helper str
