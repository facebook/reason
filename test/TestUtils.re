let lsDir = dir =>
  Sys.readdir(dir) |> Array.to_list |> List.map(Filename.concat(dir));

let syscall = (~env=[||], cmd) => {
  let (ic, oc, ec) = Unix.open_process_full(cmd, env);
  let buf1 = Buffer.create(96)
  and buf2 = Buffer.create(48);
  try(
    while (true) {
      Buffer.add_channel(buf1, ic, 1);
    }
  ) {
  | End_of_file => ()
  };
  try(
    while (true) {
      Buffer.add_channel(buf2, ec, 1);
    }
  ) {
  | End_of_file => ()
  };
  let _exit_status = Unix.close_process_full((ic, oc, ec));
  (Buffer.contents(buf1), Buffer.contents(buf2));
};

let refmtBin = "_esy/default/build/install/default/bin/refmt";
let oprintTestBin = "_esy/default/build/install/default/bin/testOprint.exe";
