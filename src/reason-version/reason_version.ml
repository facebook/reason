(**
 * Tracks the version of Reason per file, and provides supported
 * feature lookup per version.
 *)
open Reason_migrate_parsetree
open OCaml_408.Ast
open Parsetree
open Location
open Asttypes
open Ast_helper

type file_version = {
  major : int;
  minor : int;
}

type package_version = {
  major : int;
  minor : int;
  patch : int;
}

type feature =
  | AngleBracketTypes

(**
 * Tracks the current package version of Reason parser/printer. This is
 * primarily for printing the version with `refmt --version`.
 *)
let package_version = {
  major = 3;
  minor = 7;
  patch = 0;
}

let package_version_string =
  (string_of_int package_version.major) ^
  "." ^
  (string_of_int package_version.minor) ^
  "." ^
  (string_of_int  package_version.patch)

(**
 * Tracks the file version recorded in attribute. Defaults to 3.6 -
 * the version before Reason's refmt began recording versions in
 * editor formatting.
 *)
let explicit_file_version = {contents = None}

(** We start out with an inferred file version of 3.6, the last minor version
 * that did not format a version into the file.  *)
let infered_file_version = {contents = {major = 3; minor = 6;}}

let set_explicit (major, minor) =
  explicit_file_version.contents <- Some {major; minor}

let effective () = match explicit_file_version.contents with
  | Some efv -> efv
  | None -> infered_file_version.contents

let within
  ~inclusive:lower_inclusive
  (low_mjr, low_mnr)
  ~inclusive:upper_inclusive
  (up_mjr, up_mnr) =
    let ev = effective () in
    let mjr, mnr = ev.major, ev.minor in
    let lower_meets =
      if lower_inclusive then mjr > low_mjr || (mjr == low_mjr && mnr >= low_mnr)
      else mjr > low_mjr || (mjr == low_mjr && mnr > low_mnr)
    in
    let upper_meets =
      if upper_inclusive then mjr < up_mjr || (mjr == up_mjr && mnr <= up_mnr)
      else mjr < up_mjr || (mjr == up_mjr && mnr < up_mnr)
    in
    lower_meets && upper_meets

let at_least (major, minor) =
  within ~inclusive:true (major, minor) ~inclusive:true (10000,0)

let supports = function
  | AngleBracketTypes -> at_least (3, 8)


let dummy_loc () = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos;
  loc_ghost = false;
}

(* Implementation of String.split_on_char, since it's not available in older
 * OCamls *)
let _split_on_char sep_char str =
  let r = {contents = []} in
  let j = {contents = String.length str} in
  for i = String.length str - 1 downto 0 do
    if String.unsafe_get str i = sep_char then begin
      r.contents <- String.sub str (i + 1) (!j - i - 1) :: r.contents;
      j.contents <- i
    end
  done;
  String.sub str 0 j.contents :: r.contents

module Ast_nodes = struct
  let mk_warning_attribute_payload ~loc msg =
    let exp = Exp.mk ~loc  (Pexp_constant (Pconst_string(msg, None))) in
    let item = { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc } in
    PStr [item]

  let mk_version_attr_payload major minor =
    let major, minor = string_of_int major, string_of_int minor in
    let loc = dummy_loc () in
    let exp = Exp.mk ~loc  (Pexp_constant (Pconst_float(major ^ "." ^ minor, None))) in
    let item = { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc } in
    PStr [item]

  (** Creates an attribute to inject into the AST if it was not already present *)
  let inject_attr_from_version itms ~insert_after ~creator =
    let loc = dummy_loc () in
    match explicit_file_version.contents with
    | None ->
      let major, minor = package_version.major, package_version.minor in
      let attr_payload = mk_version_attr_payload major minor in
      let created = (creator ~loc {attr_name={loc; txt="reason.version"}; attr_payload; attr_loc=loc}) in
      (match itms with
      | first :: rest when insert_after first ->
        first :: created :: rest
      | _ -> created :: itms
      )
    | Some efv -> begin
        if efv.major > package_version.major ||
            (efv.major == package_version.major && efv.minor > package_version.minor) then
          let efv_mjr = string_of_int efv.major in
          let efv_mnr = string_of_int efv.minor in
          let pkg_mjr = string_of_int package_version.major in
          let pkg_mnr = string_of_int package_version.minor in
          let msg =
            "This file specifies a reason.version " ^ efv_mjr ^ "." ^ efv_mnr ^
            " which is greater than the package version " ^ pkg_mjr ^ "." ^ pkg_mnr ^
            " Either upgrade the Reason package or lower the version specified in [@reason.version ]." in
          (* let loc = match itms with *)
          (* | hd :: _ -> hd.pstr_loc *)
          (* | [] -> loc *)
          (* in *)
          let attr_payload = mk_warning_attribute_payload ~loc msg in
          let created = (creator ~loc {attr_name={loc; txt="ocaml.ppwarn"}; attr_payload; attr_loc=loc}) in
          created :: itms
        else itms
    end

  let inject_attr_from_version_impl itms =
    let insert_after = function
      | {pstr_desc = Pstr_attribute {attr_name = {txt="ocaml.doc"|"ocaml.text"; _}; _}; _} -> true
      | _ -> false
    in
    let creator = (fun ~loc x -> Str.mk ~loc (Pstr_attribute x)) in
    inject_attr_from_version itms ~insert_after ~creator

  let inject_attr_from_version_intf itms =
    let insert_after = function
      | {psig_desc = Psig_attribute {attr_name = {txt="ocaml.doc"|"ocaml.text"; _}; _}; _} -> true
      | _ -> false
    in
    let creator = (fun ~loc x -> Sig.mk ~loc (Psig_attribute x)) in
    inject_attr_from_version itms ~insert_after ~creator

  let extract_version_attribute_structure_item structure_item =
    (match structure_item with
    | {pstr_desc=(Pstr_attribute {
        attr_name={txt="reason.version"; _};
        attr_payload = PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_float(v, _)); _},_); _}];
        _
      }); _} ->
       (match _split_on_char '.' v with
       | [maj] | [maj; ""] -> Some (int_of_string maj, 0)
       | maj :: mnr :: _ -> Some (int_of_string maj, int_of_string mnr)
       | _ -> None);
    | _ -> None)

  let extract_version_attribute_signature_item sig_item =
    (match sig_item with
    | {psig_desc=(Psig_attribute {
        attr_name={txt="reason.version"; _};
        attr_payload = PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_float(v, _)); _},_); _}];
        _
      }); _} ->
       (match _split_on_char '.' v with
       | [maj] | [maj; ""] -> Some (int_of_string maj, 0)
       | maj :: mnr :: _ -> Some (int_of_string maj, int_of_string mnr)
       | _ -> None);
    | _ -> None)
end
