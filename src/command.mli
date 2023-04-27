(** Parsing of player commands. *)

type object_tuple = string * int
(** The type [object_tuple] represents a tuple that can be a part of the player
    command. The first element of the tuple is a string. The string is a
    consecutive sequence of characters and/or underscores. The second element of
    the tuple is an integer. The integer represents a valid card number in the
    deck. The integer must be between 1 and 13, inclusive. *)

(** The type [command] represents a player command that consists of a verb and an object_tuple. Invariant: the [object_tuple] carried by Request] must not be empty. *)
type command =
  | Request of object_tuple
  | Quit

exception Unrecognized
(** Raised when an unrecognized command is parsed. *)

exception Empty
(** Raised when an empty command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command]. The first word, which
    is a consecutive sequence of non-space characters, of [str] becomes the
    verb. The rest of the string becomes the object tuple.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Unrecognized] if the command is unrecognized. A command is
    unrecognized if the verb is not "request" and there is an empty object
    phrase. *)
