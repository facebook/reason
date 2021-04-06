/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/*
    let str = "@[.... some formatting ....@\n\010@.";
 */

let str = "@[.... some formatting ....@\n\010@.";
let str = {abcd|@[.... some formatting ....@\n\010@.|abcd};

let utf8_string = "😁";

let keep_representation = "\n
\t . this should be on a new line\
   ^ this should be aligned with the .
";
