open Format

type wrap = [
  | `Wrap_atoms
  | `Always_wrap
  | `Never_wrap
  | `Force_breaks
  | `Force_breaks_rec
  | `No_breaks
]

type label_break = [
  | `Auto
  | `Always
  | `Always_rec
  | `Never
]

type style_name = string
type style = {
  tag_open : string;
  tag_close : string
}

type atom_param = {
  atom_style : style_name option;
}

let atom = {
  atom_style = None
}

type list_param = {
  space_after_opening : bool;
  space_after_separator : bool;
  space_before_separator : bool;
  separators_stick_left : bool;
  space_before_closing : bool;
  stick_to_label : bool;
  align_closing : bool;
  wrap_body : wrap;
  indent_body : int;
  list_style : style_name option;
  opening_style : style_name option;
  body_style : style_name option;
  separator_style : style_name option;
  closing_style : style_name option;
}

let list = {
  space_after_opening = true;
  space_after_separator = true;
  space_before_separator = false;
  separators_stick_left = true;
  space_before_closing = true;
  stick_to_label = true;
  align_closing = true;
  wrap_body = `Wrap_atoms;
  indent_body = 2;
  list_style = None;
  opening_style = None;
  body_style = None;
  separator_style = None;
  closing_style = None;
}

type label_param = {
  label_break: label_break;
  space_after_label : bool;
  indent_after_label : int;
  label_style : style_name option;
}

let label = {
  label_break = `Auto;
  space_after_label = true;
  indent_after_label = 2;
  label_style = None;
}

type t =
    Atom of string * atom_param
  | List of (string * string * string * list_param) * t list
  | Label of (t * label_param) * t
  | Custom of (formatter -> unit)

type escape =
    [ `None
    | `Escape of
        ((string -> int -> int -> unit) -> string -> int -> int -> unit)
    | `Escape_string of (string -> string) ]

type styles = (style_name * style) list

(*
   Transform a tree starting from the leaves, propagating and merging
   accumulators until reaching the root.
*)
let propagate_from_leaf_to_root
  ~init_acc  (* create initial accumulator for a leaf *)
  ~merge_acc (* merge two accumulators coming from child nodes *)
  ~map_node  (* (node, acc) -> (node, acc) *)
  x =

  let rec aux x =
    match x with
    | Atom _ ->
        let acc = init_acc x in
        map_node x acc
    | List (param, children) ->
        let new_children, accs = List.split (List.map aux children) in
        let acc = List.fold_left merge_acc (init_acc x) accs in
        map_node (List (param, new_children)) acc
    | Label ((x1, param), x2) ->
        let acc0 = init_acc x in
        let new_x1, acc1 = aux x1 in
        let new_x2, acc2 = aux x2 in
        let acc = merge_acc (merge_acc acc0 acc1) acc2 in
        map_node (Label ((new_x1, param), new_x2)) acc
    | Custom _ ->
        let acc = init_acc x in
        map_node x acc
  in
  aux x

(*
   Convert wrappable lists into vertical lists if any of their descendants
   has the attribute wrap_body = `Force_breaks_rec.
*)
let propagate_forced_breaks x =
  (* acc = whether to force breaks in wrappable lists or labels *)
  let init_acc = function
    | List ((_, _, _, { wrap_body = `Force_breaks_rec }), _)
    | Label ((_, { label_break = `Always_rec }), _) -> true
    | Atom _
    | Label _
    | Custom _
    | List _ -> false
  in
  let merge_acc force_breaks1 force_breaks2 =
    force_breaks1 || force_breaks2
  in
  let map_node x force_breaks =
    match x with
    | List ((_, _, _, { wrap_body = `Force_breaks_rec }), _) -> x, true
    | List ((_, _, _, { wrap_body = `Force_breaks }), _) -> x, force_breaks

    | List ((op, sep, cl, ({ wrap_body = (`Wrap_atoms
                                         | `Never_wrap
                                         | `Always_wrap) } as p)),
            children) ->
        if force_breaks then
          let p = { p with wrap_body = `Force_breaks } in
          List ((op, sep, cl, p), children), true
        else
          x, false

    | Label ((a, ({ label_break = `Auto } as lp)), b) ->
        if force_breaks then
          let lp = { lp with label_break = `Always } in
          Label ((a, lp), b), true
        else
          x, false

    | List ((_, _, _, { wrap_body = `No_breaks }), _)
    | Label ((_, { label_break = (`Always | `Always_rec | `Never) }), _)
    | Atom _
    | Custom _ -> x, force_breaks
  in
  let new_x, forced_breaks =
    propagate_from_leaf_to_root
      ~init_acc
      ~merge_acc
      ~map_node
      x
  in
  new_x

module Pretty =
struct
  (*
     Rewrite the tree to be printed.
     Currently, this is used only to handle `Force_breaks_rec.
  *)
  let rewrite x = propagate_forced_breaks x

  (*
    Relies on the fact that mark_open_tag and mark_close_tag
    are called exactly once before calling pp_output_string once.
    It's a reasonable assumption although not guaranteed by the
    documentation of the Format module.
  *)
  let set_escape fmt escape =
    let print0, flush0 = pp_get_formatter_output_functions fmt () in
    let tagf0 = pp_get_formatter_tag_functions fmt () in

    let is_tag = ref false in

    let mot tag =
      is_tag := true;
      tagf0.mark_open_tag tag
    in

    let mct tag =
      is_tag := true;
      tagf0.mark_close_tag tag
    in

    let print s p n =
      if !is_tag then
        (print0 s p n;
         is_tag := false)
      else
        escape print0 s p n
    in

    let tagf = {
      tagf0 with
        mark_open_tag = mot;
        mark_close_tag = mct
    }
    in
    pp_set_formatter_output_functions fmt print flush0;
    pp_set_formatter_tag_functions fmt tagf


  let set_escape_string fmt esc =
    let escape print s p n =
      let s0 = String.sub s p n in
      let s1 = esc s0 in
      print s1 0 (String.length s1)
    in
    set_escape fmt escape


  let define_styles fmt escape l =
    if l <> [] then (
      pp_set_tags fmt true;
      let tbl1 = Hashtbl.create (2 * List.length l) in
      let tbl2 = Hashtbl.create (2 * List.length l) in
      List.iter (
        fun (style_name, style) ->
          Hashtbl.add tbl1 style_name style.tag_open;
          Hashtbl.add tbl2 style_name style.tag_close
      ) l;
      let mark_open_tag style_name =
        try Hashtbl.find tbl1 style_name
        with Not_found -> ""
      in
      let mark_close_tag style_name =
        try Hashtbl.find tbl2 style_name
        with Not_found -> ""
      in

      let tagf = {
        (pp_get_formatter_tag_functions fmt ()) with
          mark_open_tag = mark_open_tag;
          mark_close_tag = mark_close_tag
      }
      in
      pp_set_formatter_tag_functions fmt tagf
    );

    (match escape with
         `None -> ()
       | `Escape esc -> set_escape fmt esc
       | `Escape_string esc -> set_escape_string fmt esc)


  let pp_open_xbox fmt p indent =
    match p.wrap_body with
	`Always_wrap
      | `Never_wrap
      | `Wrap_atoms -> pp_open_hvbox fmt indent
      | `Force_breaks
      | `Force_breaks_rec -> pp_open_vbox fmt indent
      | `No_breaks -> pp_open_hbox fmt ()

  let extra_box p l =
    let wrap =
      match p.wrap_body with
          `Always_wrap -> true
        | `Never_wrap
        | `Force_breaks
        | `Force_breaks_rec
        | `No_breaks -> false
        | `Wrap_atoms ->
            List.for_all (function Atom _ -> true | _ -> false) l
    in
    if wrap then
      ((fun fmt -> pp_open_hovbox fmt 0),
       (fun fmt -> pp_close_box fmt ()))
    else
      ((fun fmt -> ()),
       (fun fmt -> ()))


  let pp_open_nonaligned_box fmt p indent l =
    match p.wrap_body with
        `Always_wrap -> pp_open_hovbox fmt indent
      | `Never_wrap -> pp_open_hvbox fmt indent
      | `Wrap_atoms ->
          if List.for_all (function Atom _ -> true | _ -> false) l then
            pp_open_hovbox fmt indent
          else
            pp_open_hvbox fmt indent
      | `Force_breaks
      | `Force_breaks_rec -> pp_open_vbox fmt indent
      | `No_breaks -> pp_open_hbox fmt ()


  let open_tag fmt = function
      None -> ()
    | Some s -> pp_open_tag fmt s

  let close_tag fmt = function
      None -> ()
    | Some _ -> pp_close_tag fmt ()

  let tag_string fmt o s =
    match o with
        None -> pp_print_string fmt s
      | Some tag ->
          pp_open_tag fmt tag;
          pp_print_string fmt s;
          pp_close_tag fmt ()

  let rec fprint_t fmt = function
      Atom (s, p) ->
        tag_string fmt p.atom_style s;

    | List ((_, _, _, p) as param, l) ->
        open_tag fmt p.list_style;
        if p.align_closing then
          fprint_list fmt None param l
        else
          fprint_list2 fmt param l;
        close_tag fmt p.list_style

    | Label (label, x) -> fprint_pair fmt label x
    | Custom f -> f fmt

  and fprint_list_body_stick_left fmt p sep hd tl =
    open_tag fmt p.body_style;
    fprint_t fmt hd;
    List.iter (
      fun x ->
        if p.space_before_separator then
          pp_print_string fmt " ";
        tag_string fmt p.separator_style sep;
        if p.space_after_separator then
          pp_print_space fmt ()
        else
          pp_print_cut fmt ();
        fprint_t fmt x
    ) tl;
    close_tag fmt p.body_style

  and fprint_list_body_stick_right fmt p sep hd tl =
    open_tag fmt p.body_style;
    fprint_t fmt hd;
    List.iter (
      fun x ->
        if p.space_before_separator then
          pp_print_space fmt ()
        else
          pp_print_cut fmt ();
        tag_string fmt p.separator_style sep;
        if p.space_after_separator then
          pp_print_string fmt " ";
        fprint_t fmt x
    ) tl;
    close_tag fmt p.body_style

  and fprint_opt_label fmt = function
      None -> ()
    | Some (lab, lp) ->
        open_tag fmt lp.label_style;
        fprint_t fmt lab;
        close_tag fmt lp.label_style;
        if lp.space_after_label then
          pp_print_string fmt " "

  (* Either horizontal or vertical list *)
  and fprint_list fmt label ((op, sep, cl, p) as param) = function
      [] ->
        fprint_opt_label fmt label;
        tag_string fmt p.opening_style op;
        if p.space_after_opening || p.space_before_closing then
          pp_print_string fmt " ";
        tag_string fmt p.closing_style cl

    | hd :: tl as l ->

        if tl = [] || p.separators_stick_left then
          fprint_list_stick_left fmt label param hd tl l
        else
          fprint_list_stick_right fmt label param hd tl l


  and fprint_list_stick_left fmt label (op, sep, cl, p) hd tl l =
    let indent = p.indent_body in
    pp_open_xbox fmt p indent;
    fprint_opt_label fmt label;

    tag_string fmt p.opening_style op;

    if p.space_after_opening then
      pp_print_space fmt ()
    else
      pp_print_cut fmt ();

    let open_extra, close_extra = extra_box p l in
    open_extra fmt;
    fprint_list_body_stick_left fmt p sep hd tl;
    close_extra fmt;

    if p.space_before_closing then
      pp_print_break fmt 1 (-indent)
    else
      pp_print_break fmt 0 (-indent);
    tag_string fmt p.closing_style cl;
    pp_close_box fmt ()

  and fprint_list_stick_right fmt label (op, sep, cl, p) hd tl l =
    let base_indent = p.indent_body in
    let sep_indent =
      String.length sep + (if p.space_after_separator then 1 else 0)
    in
    let indent = base_indent + sep_indent in

    pp_open_xbox fmt p indent;
    fprint_opt_label fmt label;

    tag_string fmt p.opening_style op;

    if p.space_after_opening then
      pp_print_space fmt ()
    else
      pp_print_cut fmt ();

    let open_extra, close_extra = extra_box p l in
    open_extra fmt;

    fprint_t fmt hd;
    List.iter (
      fun x ->
        if p.space_before_separator then
          pp_print_break fmt 1 (-sep_indent)
        else
          pp_print_break fmt 0 (-sep_indent);
        tag_string fmt p.separator_style sep;
        if p.space_after_separator then
          pp_print_string fmt " ";
        fprint_t fmt x
    ) tl;

    close_extra fmt;

    if p.space_before_closing then
      pp_print_break fmt 1 (-indent)
    else
      pp_print_break fmt 0 (-indent);
    tag_string fmt p.closing_style cl;
    pp_close_box fmt ()



  (* align_closing = false *)
  and fprint_list2 fmt (op, sep, cl, p) = function
      [] ->
        tag_string fmt p.opening_style op;
        if p.space_after_opening || p.space_before_closing then
          pp_print_string fmt " ";
        tag_string fmt p.closing_style cl

    | hd :: tl as l ->
        tag_string fmt p.opening_style op;
        if p.space_after_opening then
          pp_print_string fmt " ";

        pp_open_nonaligned_box fmt p 0 l ;
        if p.separators_stick_left then
          fprint_list_body_stick_left fmt p sep hd tl
        else
          fprint_list_body_stick_right fmt p sep hd tl;
        pp_close_box fmt ();

        if p.space_before_closing then
          pp_print_string fmt " ";
        tag_string fmt p.closing_style cl


  (* Printing a label:value pair.

     The opening bracket stays on the same line as the key, no matter what,
     and the closing bracket is either on the same line
     or vertically aligned with the beginning of the key.
  *)
  and fprint_pair fmt ((lab, lp) as label) x =
    match x with
        List ((op, sep, cl, p), l) when p.stick_to_label && p.align_closing ->
          fprint_list fmt (Some label) (op, sep, cl, p) l

      | _ ->
          let indent = lp.indent_after_label in
          pp_open_hvbox fmt 0;

          open_tag fmt lp.label_style;
          fprint_t fmt lab;
          close_tag fmt lp.label_style;

          (match lp.label_break with
           | `Auto ->
               if lp.space_after_label then
                 pp_print_break fmt 1 indent
               else
                 pp_print_break fmt 0 indent
           | `Always
           | `Always_rec ->
               pp_force_newline fmt ();
               pp_print_string fmt (String.make indent ' ')
           | `Never ->
               if lp.space_after_label then
                 pp_print_char fmt ' '
               else
                 ()
          );
          fprint_t fmt x;
          pp_close_box fmt ()

  let to_formatter fmt x =
    let x = rewrite x in
    fprint_t fmt x;
    pp_print_flush fmt ()

  let to_buffer ?(escape = `None) ?(styles = []) buf x =
    let fmt = Format.formatter_of_buffer buf in
    define_styles fmt escape styles;
    to_formatter fmt x

  let to_string ?escape ?styles x =
    let buf = Buffer.create 500 in
    to_buffer ?escape ?styles buf x;
    Buffer.contents buf

  let to_channel ?(escape = `None) ?(styles = []) oc x =
    let fmt = formatter_of_out_channel oc in
    define_styles fmt escape styles;
    to_formatter fmt x

  let to_stdout ?escape ?styles x = to_channel ?escape ?styles stdout x
  let to_stderr ?escape ?styles x = to_channel ?escape ?styles stderr x

end




module Compact =
struct
  open Printf

  let rec fprint_t buf = function
      Atom (s, _) -> Buffer.add_string buf s
    | List (param, l) -> fprint_list buf param l
    | Label (label, x) -> fprint_pair buf label x
    | Custom f ->
        (* Will most likely not be compact *)
        let fmt = formatter_of_buffer buf in
        f fmt;
        pp_print_flush fmt ()

  and fprint_list buf (op, sep, cl, _) = function
      [] -> bprintf buf "%s%s" op cl
    | x :: tl ->
        Buffer.add_string buf op;
        fprint_t buf x;
        List.iter (
          fun x ->
            Buffer.add_string buf sep;
            fprint_t buf x
        ) tl;
        Buffer.add_string buf cl

  and fprint_pair buf (label, _) x =
    fprint_t buf label;
    fprint_t buf x


  let to_buffer buf x = fprint_t buf x

  let to_string x =
    let buf = Buffer.create 500 in
    to_buffer buf x;
    Buffer.contents buf

  let to_formatter fmt x =
    let s = to_string x in
    Format.fprintf fmt "%s" s;
    pp_print_flush fmt ()

  let to_channel oc x =
    let buf = Buffer.create 500 in
    to_buffer buf x;
    Buffer.output_buffer oc buf

  let to_stdout x = to_channel stdout x
  let to_stderr x = to_channel stderr x
end




(* Obsolete *)
module Param =
struct
  let list_true = {
    space_after_opening = true;
    space_after_separator = true;
    space_before_separator = true;
    separators_stick_left = true;
    space_before_closing = true;
    stick_to_label = true;
    align_closing = true;
    wrap_body = `Wrap_atoms;
    indent_body = 2;
    list_style = None;
    opening_style = None;
    body_style = None;
    separator_style = None;
    closing_style = None;
  }

  let list_false = {
    space_after_opening = false;
    space_after_separator = false;
    space_before_separator = false;
    separators_stick_left = false;
    space_before_closing = false;
    stick_to_label = false;
    align_closing = false;
    wrap_body = `Wrap_atoms;
    indent_body = 2;
    list_style = None;
    opening_style = None;
    body_style = None;
    separator_style = None;
    closing_style = None;
  }

  let label_true = {
    label_break = `Auto;
    space_after_label = true;
    indent_after_label = 2;
    label_style = None;
  }

  let label_false = {
    label_break = `Auto;
    space_after_label = false;
    indent_after_label = 2;
    label_style = None;
  }
end
