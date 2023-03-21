open Printf

type player = {
  name : string;
  ready : bool;
  id : int;
  hand : int list;
  score : int;
  won_cards : int list;
}

type state = {
  deck : int list;
  players : player list;
  current_player : int;
      (*Could potentially swap this to player but this might be a little easier
        to change*)
}

exception Temporary

(* let initialize_deck = let deck : int list = [] in for i = 1 to 13 do for j =
   1 to 4 do deck @ [ i ]; print_int (List.length deck) done done *)

let initialize_deck : int list =
  [
    1;
    1;
    1;
    1;
    2;
    2;
    2;
    2;
    3;
    3;
    3;
    3;
    4;
    4;
    4;
    4;
    5;
    5;
    5;
    5;
    6;
    6;
    6;
    6;
    7;
    7;
    7;
    7;
    8;
    8;
    8;
    8;
    9;
    9;
    9;
    9;
    10;
    10;
    10;
    10;
    11;
    11;
    11;
    11;
    12;
    12;
    12;
    12;
    13;
    13;
    13;
    13;
  ]

let fake_list =
  [
    3;
    1;
    10;
    7;
    3;
    2;
    6;
    2;
    12;
    1;
    3;
    2;
    11;
    11;
    4;
    4;
    13;
    5;
    5;
    9;
    6;
    6;
    2;
    6;
    7;
    1;
    9;
    7;
    8;
    8;
    10;
    8;
    9;
    5;
    9;
    7;
    10;
    8;
    10;
    1;
    4;
    11;
    4;
    13;
    12;
    3;
    12;
    12;
    13;
    5;
    11;
    13;
  ]

(* let shuffle_deck a = let n = Array.length a in let a = Array.copy a in for i
   = n - 1 downto 1 do let k = Random.int (i + 1) in let x = a.(k) in a.(k) <-
   a.(i); a.(i) <- x done; a *)

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let rec initialize_players_hands state players deck =
  match players with
  | [ h ] -> (
      match deck with
      | h1 :: h2 :: h3 :: h4 :: h5 :: q ->
          {
            state with
            players = [ { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand } ];
            deck = q;
          }
      | _ -> raise Temporary)
  | h :: t -> (
      match deck with
      | h1 :: h2 :: h3 :: h4 :: h5 :: q ->
          initialize_players_hands
            {
              state with
              players =
                [ { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand } ];
              deck = q;
            }
            t q
      | _ -> raise Temporary)
  | [] -> raise Temporary
