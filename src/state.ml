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
