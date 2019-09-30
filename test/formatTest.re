open TestFramework;
open TestUtils;

let buildRefmtArgs = filename =>
  refmtBin ++ " --print-width 50 --print re " ++ filename;

let buildOprintArgs = filename => "cat " ++ filename ++ " | " ++ oprintTestBin;

describe("formatTest", ({describe, _}) => {
  ["idempotentTests", "typeCheckedTests", "unit_tests"]
  |> List.iter(folder =>
       describe(folder, ({test, _}) =>
         lsDir("./formatTest/" ++ folder ++ "/input")
         |> List.iter(filename =>
              test(
                filename,
                ({expect}) => {
                  let (stdOut, stdErr) = syscall(buildRefmtArgs(filename));
                  expect.string(stdOut).toMatchSnapshot();
                  expect.string(stdErr).toBeEmpty();
                },
              )
            )
       )
     );

  describe("errorTests", ({test, _}) =>
    lsDir("./formatTest/errorTests/input")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let (stdOut, stdErr) = syscall(buildRefmtArgs(filename));
             expect.string(stdErr).toMatchSnapshot();
             expect.string(stdOut).toBeEmpty();
           },
         )
       )
  );

  describe("oprintTests", ({test, _}) =>
    lsDir("./formatTest/oprintTests/input")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let (stdOut, stdErr) = syscall(buildOprintArgs(filename));
             expect.string(stdOut).toMatchSnapshot();
             expect.string(stdErr).toBeEmpty();
           },
         )
       )
  );
});
