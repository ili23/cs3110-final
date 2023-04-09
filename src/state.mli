(** Representation of a game state

    This module represents the state of a game of Go-Fish including player
    details (such as their name, their hand, and the cards they won already),
    the current turn's player, and the game deck. *)

type player
(** The abstract type of values representing a playe. *)

type state
(** The abstract type of values representing a game state. *)

val init_player : string -> player
(** [init_player a] creates a player with the name a. *)

val add_player : state -> player -> state
(** [add_player st pl] adds player pl to state st. *)

val getPlayerHand : player -> int list
(** [getPlayerHand pl] shows player pl's hand. *)

val getPlayerName : player -> string
(** [getPlayerHand pl] shows player pl's name. *)

val init_state : state
(** [init_state] creates a new game with an empty deck and no players*)

val getPlayerList : state -> player list
(** [getPlayerList st] shows the players in the game*)

val initialize_players_hands : player list -> int list -> player list
val updatePlayers : state -> player list -> state
