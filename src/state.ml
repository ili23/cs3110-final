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
}

exception Filler
exception Temporary

let init_player name =
  { name; ready = false; id = -1; hand = []; score = 0; won_cards = [] }

let init_state = { deck = []; players = []; current_player = 0 }
let add_cards game deck = { game with deck }
let get_player_name player = player.name

let add_player game_state player =
  { game_state with players = game_state.players @ [ player ] }

let rec assign_id_rec num p_list : player list =
  match p_list with
  | [] -> p_list
  | h :: t -> assign_id_rec (num + 1) ({ h with id = num } :: t)

let assign_id game_state num =
  { game_state with players = assign_id_rec num game_state.players }

let next_turn game_state num =
  { game_state with current_player = (game_state.current_player + 1) mod num }

let rec check_hand hand (card : int) =
  match hand with
  | [] -> false
  | h :: t -> if h == card then true else check_hand t card

let check_person player card = check_hand player.hand card

let add_card player card =
  { player with hand = List.sort compare (card :: player.hand) }

let drawFromPile game player =
  match game.deck with
  | [] -> player
  | h :: t -> { player with hand = h :: player.hand }

(*The following functions are used to transfer cards from one player to
  another*)
let count_cards card player =
  List.length (List.filter (fun x -> x = card) player.hand)

let delete_cards card player =
  { player with hand = List.filter (fun x -> x <> card) player.hand }

let rec repeat_add_card player card = function
  | 0 -> player
  | x -> repeat_add_card (add_card player card) card (x - 1)

(*Need to call this each time we update player hands*)
let rec update_player game player =
  match game.players with
  | [] -> game
  | h :: t ->
      if h = player then { game with players = player :: t }
      else update_player game player

let update_players game p_list = { game with players = p_list }
let get_player_list game = game.players
let get_player_hand player = List.sort compare player.hand

let rec check_quad_helper lst prev cnt =
  match lst with
  | [] -> if cnt = 4 then Some prev else None
  | h :: t ->
      if cnt = 4 then Some prev
      else if h = prev then check_quad_helper t prev (cnt + 1)
      else check_quad_helper t h 1

let check_quad player = check_quad_helper player.hand 0 0

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

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let gen_random_int bound =
  Random.self_init ();
  Random.int bound

let rec initialize_players_hands players deck =
  match players with
  | [ h ] -> (
      match deck with
      | h1 :: h2 :: h3 :: h4 :: h5 :: q ->
          [ { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand } ]
      | _ -> raise Temporary)
  | h :: t -> (
      match deck with
      | h1 :: h2 :: h3 :: h4 :: h5 :: q ->
          { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand }
          :: initialize_players_hands t q
      | _ -> raise Temporary)
  | [] -> raise Temporary

(* let rec initialize_players_hands state players deck = match players with | [
   h ] -> ( match deck with | h1 :: h2 :: h3 :: h4 :: h5 :: q -> { state with
   players = [ { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand } ]; deck =
   q; } | _ -> raise Temporary) | h :: t -> ( match deck with | h1 :: h2 :: h3
   :: h4 :: h5 :: q -> initialize_players_hands { state with players =
   state.players @ [ { h with hand = h1 :: h2 :: h3 :: h4 :: h5 :: h.hand } ];
   deck = q; } t q | _ -> raise Temporary) | [] -> raise Temporary *)
