module Comment = Reason_comment

module Range = struct
  (** [t] represents an interval, including endpoints,
   * delimited by two linenumbers. *)
  type t = {
    lnum_start: int;
    lnum_end: int
  }

  (*
   * make a range delimited by [loc1] and [loc2]
   * 1| let a = 1;
   * 2|
   * 3|
   * 4|
   * 5| let b = 2;
   * If loc1 represents `let a = 1` and loc2 represents `let b = 2`,
   * we get the range: {lnum_start: 2; lnum_end 4}
   *)
  let makeRangeBetween loc1 loc2 = Location.{
    lnum_start = loc1.loc_end.pos_lnum + 1;
    lnum_end = loc2.loc_start.pos_lnum - 1;
  }

  (** check whether [range] contains the [loc] *)
  let containsLoc range loc =
    let open Location in
    range.lnum_start <= loc.loc_start.pos_lnum
    && range.lnum_end >= loc.loc_end.pos_lnum

  (**
   * checks if [range] contains whitespace.
   * When comments are passed, the computation
   * takes the height of the comments into account.
   *
   * Example:
   * 1| let a = 1;
   * 2|
   * 3| /* a multi-
   * 4|   line comment */
   * 5| let b = 1;
   * The range (line 2 - line 4) has whitespace.
   *
   * 1| let a = 1;
   * 2| /* a multi-
   * 3|   line comment */
   * 4| let b = 1;
   * The range (line 2 - line 3) does not have whitespace.
   *)
  let containsWhitespace ?comments ~range () =
    (* compute the amount of lines the comments occupy in the given range *)
    let h = match comments with
    | Some(comments) ->
      List.fold_left (fun acc (curr : Comment.t) ->
        let cl = Comment.location curr in
        let open Location in
        let startLnum = cl.loc_start.pos_lnum in
        let endLnum = cl.loc_end.pos_lnum in
        if containsLoc range cl then
          acc + (endLnum - startLnum + 1)
        else acc
        ) 0 comments
    | None -> 0
    in
    range.lnum_end - range.lnum_start - h >= 0
end

(** compute if there's space (one or more line) between [loc1] and [loc2] *)
let hasSpaceBetween loc1 loc2 =
  Location.(loc1.loc_start.pos_lnum - loc2.loc_end.pos_lnum) > 1

