(**
 * This module should include utilities for parsing and printing template
 * strings, but should not have any dependencies on any printing framework
 * (like Easy_format or Reason_layout). For that, make another module.  This
 * file should be shared between the printer and the parser, so avoiding
 * dependencies on printing frameworks, makes it easy to bundle just the parser
 * if necessary.
 *)

open Reason_migrate_parsetree
open Ast_408

module Parse = struct
  (* Normalizes the last line:
   *
   *     `
   *       abc
   *       123
   *       xyz<spc><spc>`
   *
   * Into:
   *
   *     `
   *       abc
   *       123
   *       xyz<spc>
   *     `
   *  ^            ^
   *  |            |
   *  |            one less space than previously (or zero if already had zero)
   *  white spaces 2 fewer than observed indent of last line.
   *
   * Or doesn't do anything if the last line is all white space.
   *
   *     `
   *       abc
   *       123
   *       xyz<spc>
   *    `
   *
   * Into:
   *
   *     `
   *       abc
   *       123
   *       xyz<spc>
   *     `
   *  ^            ^
   *  |            |
   *  |            Doesn't remove any trailing white space on last line in this case.
   *  Undisturbed
   *
   * Removes a final line filled only with whitespace, returning its indent,
   * or does not remove the final line if it has non-whitespace, but returns
   * a normalized version of that final line if it is not terminated with a newline *)
  let normalize_or_remove_last_line rev_lst =
    match rev_lst with
    | [] -> (0, [])
    | s :: tl ->
      let indent = Reason_syntax_util.num_leading_space s in
      let len = String.length s in
      if indent == len then (indent, tl)
      else
        (* Else, the last line contains non-white space after white space *)
        let withoutFinalWhite =
          (* Also, trim one single final white space *)
          match String.get s (len - 1) with
          | '\t' | ' ' | '\n' | '\r' -> String.sub s 0 (len - 1)
          | _ -> s
        in
        (indent, withoutFinalWhite :: tl)

  let rec strip_leading_for_non_last ~indent acc revLst =
    match revLst with
    | [] -> acc
    | hd::tl ->
        (* The first line doesn't get a newline before it *)
        let ln = Reason_syntax_util.trim_left ~max_trim_size:(indent+2) hd in
        let next = match tl with | [] -> ln ^ acc | _ -> "\n" ^ ln ^ acc in
        strip_leading_for_non_last ~indent next tl
end


module Print = struct
  let escape_string_template str =
    let buf = Buffer.create (String.length str) in
    let strLen = String.length str in
    String.iteri (fun i c ->
        match c with
        | '`' -> Buffer.add_string buf "\\`"
        | '$' when i + 1 < strLen && str.[i + 1] == '{' -> Buffer.add_string buf "\\$"
        | c -> Buffer.add_char buf c
      ) str;
    Buffer.contents buf
  let is_template_style lst =
    match lst with [({Parsetree.attr_name = {txt="reason.template"}; _ })] -> true | _ -> false
end
