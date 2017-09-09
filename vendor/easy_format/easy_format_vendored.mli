(**
   Easy_format: indentation made easy.
*)

(**
  This module provides a functional, simplified layer over
  the Format module of the standard library.

  Input data must be first modelled as a tree using 3 kinds of nodes:
  - atoms
  - lists
  - labelled nodes

  Atoms represent any text that is guaranteed to be printed as-is.
  Lists can model any sequence of items such as arrays of data
  or lists of definitions that are labelled with something
  like "int main", "let x =" or "x:".
*)

type wrap =
    [ `Wrap_atoms
    | `Always_wrap
    | `Never_wrap
    | `Force_breaks
    | `Force_breaks_rec
    | `No_breaks ]
(** List wrapping conditions:
    - [`Wrap_atoms]: wrap if the list contains only atoms
    - [`Always_wrap]: always wrap when needed
    - [`Never_wrap]: never wrap,
      i.e. the list is either horizontal or vertical
    - [`Force_breaks]: align vertically,
      i.e. always break line between list items and
      align the left edge of each item.
    - [`Force_breaks_rec]: same as [`Force_breaks] but turns
      any wrappable ancestor node's wrap property ([`Wrap_atoms]
      or [`Always_wrap]) into [`Force_breaks].
    - [`No_breaks]: align horizontally,
      i.e. never break line between list items
*)

type label_break = [
  | `Auto
  | `Always
  | `Always_rec
  | `Never
]
(** When to break the line after a [Label]:
    - [Auto]: break after the label if there's not enough room
    - [Always]: always break after the label
    - [Always_rec]: always break after the label and force breaks in all parent
      lists and labels, similarly to [`Force_breaks_rec] for lists.
    - [Never]: never break after the label
*)

type style_name = string

type style = {
  tag_open : string;
  tag_close : string
}
    (** Pair of opening and closing tags that are inserted around
	text after pretty-printing. *)

type atom_param = {
  atom_style : style_name option; (** Default: [None] *)
}

val atom : atom_param


(** List-formatting parameters.
    Always derive a new set of parameters from an existing record.
    See {!Easy_format.list}.
*)
type list_param = {
  space_after_opening : bool; (** Whether there must be some whitespace
				  after the opening string.
				  Default: [true] *)
  space_after_separator : bool; (** Whether there must be some whitespace
				    after the item separators.
				    Default: [true] *)
  space_before_separator : bool; (** Whether there must be some whitespace
				     before the item separators.
				     Default: [false] *)
  separators_stick_left : bool; (** Whether the separators must
				    stick to the item on the left.
				    Default: [true] *)
  space_before_closing : bool; (** Whether there must be some whitespace
				   before the closing string.
				   Default: [true] *)
  stick_to_label : bool; (** Whether the opening string should be fused
			     with the preceding label.
			     Default: [true] *)
  align_closing : bool; (** Whether the beginning of the
			    closing string must be aligned
			    with the beginning of the opening string
			    (stick_to_label = false) or
			    with the beginning of the label if any
			    (stick_to_label = true).
			    Default: [true] *)
  wrap_body : wrap; (** Defines under which conditions the list body
			may be wrapped, i.e. allow several lines
			and several list items per line.
			Default: [`Wrap_atoms] *)
  indent_body : int; (** Extra indentation of the list body.
			 Default: [2] *)

  list_style : style_name option; (** Default: [None] *)
  opening_style : style_name option; (** Default: [None] *)
  body_style : style_name option; (** Default: [None] *)
  separator_style : style_name option; (** Default: [None] *)
  closing_style : style_name option; (** Default: [None] *)
}

val list : list_param
  (** Default list-formatting parameters, using the default values
      described in the type definition above.

      In order to make code compatible with future versions of the library,
      the record inheritance syntax should be used, e.g.
      [ { list with align_closing = false } ].
      If new record fields are added, the program would still compile
      and work as before.
  *)

(** Label-formatting parameters.
    Always derive a new set of parameters from an existing record.
    See {!Easy_format.label}.
*)
type label_param = {
  label_break: label_break;
    (** Whether to break the line after the label.
        Introduced in version 1.2.0.
        Default: [`Auto] *)

  space_after_label : bool;
    (** Whether there must be some whitespace after the label.
	Default: [true] *)

  indent_after_label : int;
    (** Extra indentation before the item that comes after a label.
	Default: [2]
    *)

  label_style : style_name option;
    (** Default: [None] *)
}

val label : label_param
  (** Default label-formatting parameters, using the default values
      described in the type definition above.

      In order to make code compatible with future versions of the library,
      the record inheritance syntax should be used, e.g.
      [ { label with indent_after_label = 0 } ].
      If new record fields are added, the program would still compile
      and work as before.
 *)



type t =
    Atom of string * atom_param (** Plain string normally
				    without line breaks. *)

  | List of
      (
	string    (* opening *)
	* string  (* separator *)
	* string  (* closing *)
	* list_param
      )
      * t list
	(** [List ((opening, separator, closing, param), nodes)] *)

  | Label of (t * label_param) * t
      (** [Label ((label, param), node)]: labelled node. *)

  | Custom of (Format.formatter -> unit)
      (** User-defined printing function that allows to use the
	  Format module directly if necessary. It is responsible
	  for leaving the formatter in a clean state. *)
(** The type of the tree to be pretty-printed. Each node contains
    its own formatting parameters.

    Detail of a list node
    [List ((opening, separator, closing, param), nodes)]:

    - [opening]: opening string such as ["\{"] ["\["] ["("] ["begin"] [""] etc.
    - [separator]: node separator such as [";"] [","] [""] ["+"] ["|"] etc.
    - [closing]: closing string such as ["\}"] ["\]"] [")"] ["end"] [""] etc.
    - [nodes]: elements of the list.

*)

type escape =
    [ `None
    | `Escape of
	((string -> int -> int -> unit) -> string -> int -> int -> unit)
    | `Escape_string of (string -> string) ]

type styles = (style_name * style) list

(** The regular pretty-printing functions *)
module Pretty :
sig
  val define_styles : Format.formatter -> escape -> styles -> unit
  val to_formatter : Format.formatter -> t -> unit

  val to_buffer : ?escape:escape -> ?styles:styles -> Buffer.t -> t -> unit
  val to_string : ?escape:escape -> ?styles:styles -> t -> string
  val to_channel : ?escape:escape -> ?styles:styles -> out_channel -> t -> unit
  val to_stdout : ?escape:escape -> ?styles:styles -> t -> unit
  val to_stderr : ?escape:escape -> ?styles:styles -> t -> unit
end

(** No spacing or newlines other than those in the input data
    or those produced by [Custom] printing. *)
module Compact :
sig
  val to_buffer : Buffer.t -> t -> unit
  val to_string : t -> string
  val to_channel : out_channel -> t -> unit
  val to_stdout : t -> unit
  val to_stderr : t -> unit
  val to_formatter : Format.formatter -> t -> unit
 end


(**/**)

(** Deprecated. Predefined sets of parameters *)
module Param :
sig
  val list_true : list_param
    (** Deprecated. All boolean fields set to true. indent_body = 2. *)

  val label_true : label_param
    (** Deprecated. All boolean fields set to true. indent_after_label = 2. *)

  val list_false : list_param
    (** Deprecated. All boolean fields set to false. indent_body = 2. *)

  val label_false : label_param
    (** Deprecated. All boolean fields set to false. indent_after_label = 2. *)
end

