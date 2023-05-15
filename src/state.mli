(** Representation of a game state

    This module represents the state of a game of Go-Fish including player
    details (such as their name, their hand, and the cards they won already),
    the current turn's player, and the game deck. *)

type player
(** The abstract type of values representing a player. *)

type state
(** The abstract type of values representing a game state. *)

type player_and_score
(** Record type representing a player and their score. *)

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

val update_player : state -> player -> player -> state
(** [update_player st pl new_pl] updates st with new_pl replacing pl*)

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

val count_cards : int -> player -> int
(** [count_cards card player] returns the number of cards of type card the
    player has. *)

val has_card : int -> player -> bool
(** [has_card card player] returns if the player has a card of type card. *)

val remove_card_top : int -> state -> state
(** [remove_card_top num st] returns a new game state with num cards removed
    from the deck from the game state st.*)

val draw_from_pile : state -> player -> player
(** [remove_top_card deck] removes the top card of a non-empty deck. If the deck
    is empty, it raises exception NoCardsLeft*)

val get_current_player : state -> player
(** [get_current_player state] returns the id of the current player of the game. *)

val exchange_cards : player -> player -> int -> state -> state
(** [exchange_cards receiver sender card game] is the new state after cards of
    type card are moved from the sender to the receiver. *)

val find_player : string -> player list -> player
(** [find_player name pl] checks if the player is in the player list of the
    game. If the player is not in the game, then NoPlayer exception is raised. *)

val get_deck : state -> int list
(** [get_deck state] returns the state of the deck. *)

val get_current_player_state : state -> int
(** [get_current_player_state st] returns the current player of the game. *)

val assign_id : state -> int -> state
(** [assign_id st num] returns the state after assigning each of the num number
    of players an id. *)

val get_id : player -> int
(** [get_id pl] returns the id of player pl. *)

val next_turn : int -> state -> state
(** [next_turn num st] returns the new state after updating state st to be the
    next turn. The number of players num determines the next player up. *)

val check_deck : state -> bool
(** [check_deck st] returns whether the the deck of state st has cards. *)

val check_hand : int list -> int -> bool
(** [check_hand hand v] returns whether the hand contains card with value v. *)

val add_card : player -> int -> player
(** [add_card pl v] returns the player after adding card v to the pl's hand. *)

val set_deck : state -> int list -> state
(** [set_deck st d] returns the state after setting st's deck to d. *)

val repeat_add_card : player -> int -> int -> player
(** [repeat_add_card pl card num] returns pl after adding card num times to pl's
    hand. *)

val delete_cards : int -> player -> player
(** [delete_cards card pl] returns pl after removing each instance of card from
    pl's hand. *)

val check_quad : player -> int list
(** [check_quad pl] returns an empty list if the player does not have 4 cards of
    the same value and a list with each of the card values that the player has 4
    cards of. *)

val add_quad : player -> player
(** [add_quad pl] returns the player pl after it is updated to have a higher
    score, the quads they have added to the list of won_cards, and those cards
    removed from their hand. *)

val get_score : player -> int
(** [get_score pl] returns the score of player pl. *)

val get_won_cards : player -> int list
(** [get_won_cards pl] returns the won_cards of player pl. *)

val get_hand : player -> int list
(** [get_hand pl] returns the hand of player pl. *)

val check_top_card : state -> int
(** [check_top_card st] returns the top card in the deck of st. *)

val get_log : state -> string list
(** [get_log st] returns the log of events for the current game state st.*)

val add_log : state -> string -> state
(** [add_log st message] returns a game state st with message added to its logs. *)

val check_winner : state -> string list
(** [check_winner st] returns the winner(s) of the game*)

val set_score : player -> int -> player
(** [set_score pl score sets the score of player pl]*)

exception NoPlayer
(** Raised when no player is found.*)

exception NoCardsLeft
(** Raised when no cards are left in the deck.*)

exception Illegal
(** Raised as an illegal state for impossible pattern match case is reached .*)

exception Temporary
(** Raised as a temp for impossible pattern match cases.*)

exception Impossible
(** Raised as a placeholder for impossible pattern match cases.*)
