(** Representation of a game state

    This module represents the state of a game of Go-Fish including player
    details (such as their name, their hand, and the cards they won already),
    the current turn's player, and the game deck. *)

type player
(** The abstract type of values representing a player. *)

type state
(** The abstract type of values representing a game state. *)

val init_player : string -> player
(** [init_player a] creates a player with the name a. *)

val init_state : state
(** [init_state] creates a new game with an empty deck and no players*)

val add_player : player -> state -> state
(** [add_player st pl] adds player pl to state st. *)

val check_person : player -> int -> bool
(** [check_person pl c] checks if player pl has card c in their hand. *)

val initialize_players_hands : int list -> player list -> player list
(** [initalize_players_hands pl deck] distributes five cards to each player in
    pl from the deck.*)

val update_players : state -> player list -> state
(** [update_players st pl] updates st with the updated pl*)

val get_player_hand : player -> int list
(** [get_player_han pl] shows player pl's hand. The hand will be sorted
    numerically. (Aces are considered the smallest in this game.)*)

val get_player_name : player -> string
(** [get_player_name pl] shows player pl's name. *)

val get_player_list : state -> player list
(** [get_player_list st] shows the players in the game*)

val shuffle : int list
(** [shuffle] is a randomly shuffled deck of cards*)

val remove_top_card : int list -> int list
(** [remove_top_card deck] removes the top card of a non-empty deck. If the deck
    is empty, it raises exception NoCardsLeft*)

val remove_cards : int -> int list -> int list
(** [remove_cards num deck] removes the top num cards from the deck. *)

val find_player : string -> player list -> player
(** [find_player name players] returns the player with name 'name'. *)
