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

let init_player name =
  { name; ready = false; id = 0; hand = []; score = 0; won_cards = [] }

let init_state cards = { deck = cards; players = []; current_player = 0 }

let add_player game_state player =
  { game_state with players = player :: game_state.players }

let rec assign_id_rec num p_list : player list =
  match p_list with
  | [] -> p_list
  | h :: t -> assign_id_rec (num + 1) ({ h with id = num } :: t)

let assign_id game_state num =
  { game_state with players = assign_id_rec num game_state.players }

(*Need to change the mod 4 to the number of players but we default to 4 players
  for now*)
let next_turn game_state =
  { game_state with current_player = (game_state.current_player + 1) mod 4 }
