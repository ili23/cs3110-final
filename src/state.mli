type player
type state

val init_player : string -> player
val add_player : state -> player -> state
val init_state : state
val getPlayerList : state -> player list
val fake_list : int list
val initialize_players_hands : player list -> int list -> player list
val updatePlayers : state -> player list -> state
val getPlayerHand : player -> int list
val getPlayerName : player -> string
