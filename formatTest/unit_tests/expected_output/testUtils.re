[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let printSection = s => {
  print_string("\n");
  print_string(s);
  print_string("\n---------------------\n");
};

let printLn = s => print_string(s ++ "\n");
