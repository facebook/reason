/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let l = [1,2,3] |> List.map (i => i+1, _) |> List.filter (i => i>0, _);

let l = (i => i+1) |> List.map(_, [1,2,3]);

let x = List.length(_);

let nested = x => List.length(_);

let incr = (~v) => v+1;

let l1 = [1,2,3] |> List.map(incr(~v=_)) |> List.length;

let l2 = [1,2,3] |> List.map(incr(~v =_)) |> List.length;

let a1 = [|1, 2, 3|] |> Array.get(_, 1);

let s1 = "roses are red" |> String.get(_, 4);

let optParam = (~v=?, ()) => v == None ? 0 : 1;

let l1 =
  [Some(1), None, Some(2)] |> List.map(optParam(~v=?_, ())) |> List.length;

let l2 =
  [Some(1), None, Some(2)] |> List.map(optParam(~v =?_, ())) |> List.length;

let argIsUnderscore1 = _ => 34;

let argIsUnderscore2 = (_ => 34);
  
let argIsUnderscore3 = _ : int => 34;
  
let argIsUnderscore4 = (_ : int => 34);

let argIsUnderscore5 = (_: int) => 34;

let argIsUnderscore6 = ((_: int) => 34);

type reasonXyz =
  | X
  | Y(int,int,int)
  | Z(int,int)
  | Q
  | R;

type reasonXyzWithOf =
  | X
  | Y(int,int,int)
  | Z(int,int)
  | Q
  | R;

let reasonBarAs = fun
  | ((Y(_) | Z(_)) as t, _) => {let _ = t; true}
  | _ => false;

let reasonDoubleBar = fun
  | X | Y(_,_,_) | Z(_,_) | Q => true
  | _ => false;

let reasonDoubleBarNested = fun
  | X | Y(_,_,_) | (Z(_,_) | Q)  => true
  | _ => false;


/* Liberal use of the Any pattern being compatible with multiple
  arguments  */
let reasonDoubleBarAnyPatterns = fun
  | X | Y(_) | Z(_) | Q => true
  | _ => false;

let reasonDoubleBarNestedAnyPatterns = fun
  | X | Y(_) | (Z(_) | Q)  => true
  | _ => false;

let (\+) = (+);

let a = 2.0 ** 4.0;

let (\===) = (===);

let expectedPrecendence = 1 + 1 \=== 1 + 1 && 1 + 1 \!== 1 + 1;

let expectedPrecendence = 1 \+ 1 \=== 1 \+ 1 && 1 \+ 1 \!== 1 \+ 1;

module X: {let x: (~x: unit=?, unit) => unit;} = {
  let x(~x=(),()) = ();
};

let display (~message=("hello": string), ~person: string="Reason", time: float) = 1;

let not = (x, y) => x + y;

let added: int = not(1, 2);

let better = foo => !foo ? 42 : not(41, 2);
