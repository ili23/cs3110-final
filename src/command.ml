type object_tuple = string * int

type command =
  | Request of object_tuple
  | Quit

exception Unrecognized
exception Empty

let rec parse_helper str =
  let lower = String.lowercase_ascii str in
  let split = String.split_on_char ' ' lower in
  let filtered = List.filter (fun item -> item <> "") split in
  match filtered with
  | [] -> raise Empty
  | [ h ] when h = "quit" -> Quit
  | [ h; t1; t2 ] when h = "request" && List.length filtered > 2 -> (
      match t2 with
      | "a" -> Request (t1, 1)
      | "j" -> Request (t1, 11)
      | "q" -> Request (t1, 12)
      | "k" -> Request (t1, 13)
      | _ -> Request (t1, int_of_string t2))
  | _ -> raise Unrecognized

let parse str = parse_helper str
let x = Quit
