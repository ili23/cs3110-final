(****************************************************************************
  Types for data structures in states.
  ***************************************************************************)
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

(****************************************************************************
  Exceptions needed for states.
  ***************************************************************************)
exception Filler
exception Temporary
exception NoCardsLeft
exception NoPlayer

(****************************************************************************
  Helper functions for general state funtions.
  ***************************************************************************)
let gen_rand_int bound =
  Random.self_init ();
  Random.int bound

let shuffle =
  let d = Array.make 52 1 in
  for x = 1 to 13 do
    for y = 0 to 3 do
      Array.set d ((4 * (x - 1)) + y) x
    done
  done;
  for x = 0 to 100 do
    let x_1 = gen_rand_int 52 in
    let x_2 = gen_rand_int 52 in
    let x_1_value = Array.get d x_1 in
    let x_2_value = Array.get d x_2 in
    Array.set d x_2 x_1_value;
    Array.set d x_1 x_2_value
  done;
  Array.to_list d

(****************************************************************************
  Functions that create/set/change states.
  ***************************************************************************)
let init_player name =
  { name; ready = false; id = -1; hand = []; score = 0; won_cards = [] }

let init_state = { deck = shuffle; players = []; current_player = 0 }

let add_player player game_state =
  { game_state with players = game_state.players @ [ player ] }

let rec assign_id_rec index p_list : player list =
  match p_list with
  | [] -> p_list
  | h :: t -> { h with id = index } :: assign_id_rec (index + 1) t

let assign_id game_state num =
  { game_state with players = assign_id_rec 0 game_state.players }

let next_turn num game_state =
  { game_state with current_player = (game_state.current_player + 1) mod num }

(****************************************************************************
  Functions that get information about states.
  ***************************************************************************)
let get_deck state = state.deck
let get_current_player_state state = state.current_player
let get_id player = player.id

let get_current_player state =
  let rec get_turn id player_lst =
    match player_lst with
    | [] -> raise NoPlayer (*Impossible*)
    | h :: t -> if h.id = id then h else get_turn id t
  in
  get_turn state.current_player state.players

let get_player_name player = player.name

(****************************************************************************
  Other functions related to states.
  ***************************************************************************)

let rec check_hand hand (card : int) =
  match hand with
  | [] -> false
  | h :: t -> if h == card then true else check_hand t card

let check_person player card = check_hand player.hand card

let check_deck state =
  match state.deck with
  | [] -> false
  | h :: t -> true

let add_card player card =
  { player with hand = List.sort compare (card :: player.hand) }

let remove_top_card_help deck =
  match deck with
  | [] -> raise NoCardsLeft
  | h :: t -> t

(** Used to remove num top cards in deck.*)
let rec remove_card_top num deck =
  match num with
  | 0 -> deck
  | x -> remove_card_top (x - 1) (remove_top_card_help deck)

let draw_from_pile game player =
  match game.deck with
  | [] -> raise NoCardsLeft
  | h :: t -> { player with hand = h :: player.hand }

(*The following functions are used to transfer cards from one player to
  another*)
let count_cards card player =
  List.length (List.filter (fun x -> x = card) player.hand)

let has_card card player = count_cards card player > 0

let delete_cards card player =
  { player with hand = List.filter (fun x -> x <> card) player.hand }

let rec repeat_add_card player card = function
  | 0 -> player
  | x -> repeat_add_card (add_card player card) card (x - 1)

(*Need to call this each time we update player hands*)

let rec update_player_list player_list player new_player =
  match player_list with
  | [] -> []
  | h :: t when h = player ->
      new_player :: update_player_list t player new_player
  | h :: t -> h :: update_player_list t player new_player

let rec update_player game player new_player =
  { game with players = update_player_list game.players player new_player }

let update_players game p_list = { game with players = p_list }
let get_player_list game = game.players
let get_player_hand player = List.sort compare player.hand

let exchange_cards receiver sender card game =
  let count = count_cards card sender in
  let new_sender = delete_cards card sender in
  let send_state = update_player game sender new_sender in
  let new_receiver = repeat_add_card receiver card count in
  update_player send_state receiver new_receiver

let rec check_quad_helper lst prev cnt acc =
  match List.sort compare lst with
  | [] -> if cnt = 4 then prev :: acc else acc
  | h :: t ->
      if cnt = 4 then check_quad_helper t h 1 (prev :: acc)
      else if h = prev then check_quad_helper t prev (cnt + 1) acc
      else check_quad_helper t h 1 acc

(** will return [] if no quads, otherwise will return nonempty list*)
let check_quad player = check_quad_helper player.hand 0 0 []

(* let initialize_deck = let deck : int list = [] in for i = 1 to 13 do for j =
   1 to 4 do deck @ [ i ]; print_int (List.length deck) done done *)

let rec initialize_players_hands deck players =
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
          :: initialize_players_hands q t
      | _ -> raise Temporary)
  | [] -> raise Temporary

let rec find_player name player_list =
  match player_list with
  | [] -> raise NoPlayer
  | h :: t -> if h.name = name then h else find_player name t
