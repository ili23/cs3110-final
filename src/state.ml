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

exception Filler

let init_player name =
  { name; ready = false; id = -1; hand = []; score = 0; won_cards = [] }

let init_state = { deck = []; players = []; current_player = 0 }
let add_cards game deck = { game with deck }

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

let rec checkHand hand (card : int) =
  match hand with
  | [] -> false
  | h :: t -> if h == card then true else checkHand t card

let checkPerson player card = checkHand player.hand card
let addCard player card = { player with hand = card :: player.hand }

let drawFromPile game player =
  match game.deck with
  | [] -> player
  | h :: t -> { player with hand = h :: player.hand }

(*The following functions are used to transfer cards from one player to
  another*)
let countCards card player =
  List.length (List.filter (fun x -> x = card) player.hand)

let deleteCards card player =
  { player with hand = List.filter (fun x -> x <> card) player.hand }

let rec repeatAddCard player card = function
  | 0 -> player
  | x -> repeatAddCard (addCard player card) card (x - 1)

(*Need to call this each time we update player hands*)
let rec updateGame game player =
  match game.players with
  | [] -> game
  | h :: t ->
      if h = player then { game with players = player :: t }
      else updateGame game player
