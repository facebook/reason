open TestFramework;
open TestUtils;

let extensions = [".ml", ".mli", ".re", ".rei"];
let isSourcefile = filename =>
  List.exists(
    extension => Filename.extension(filename) == extension,
    extensions,
  );

let buildRefmtArgs = (filename, extension) => {
  let args =
    switch (extension) {
    | ".ml" =>
      let heuristics = Filename.dirname(filename) ++ "/arity.txt";
      "--interface false --parse ml --heuristics-file " ++ heuristics;
    | ".mli" =>
      let heuristics = Filename.dirname(filename) ++ "/arity.txt";
      "--interface true --parse ml --heuristics-file " ++ heuristics;
    | ".re" => "--interface false --parse re"
    | ".rei" => "--interface true --parse re"
    | _ => ""
    };

  refmtBin ++ " --print-width 50 --print re " ++ filename ++ " " ++ args;
};

let buildOprintArgs = filename => "cat " ++ filename ++ " | " ++ oprintTestBin;

let buildOcamlCompiler = (filename, extension) => {
  let compileFlags =
    switch (extension) {
    | ".re" => "-intf-suffix .rei -impl"
    | ".rei" => "-intf"
    | _ => ""
    };

  let whichOcamlc = "which ocamlc";

  "$("
  ++ whichOcamlc
  ++ ") -c -pp '"
  ++ refmtBin
  ++ " --print binary'"
  ++ compileFlags
  ++ " "
  ++ filename;
};

describe("formatTest", ({describe, _}) => {
  ["idempotentTests", "typeCheckedTests", "unit_tests"]
  |> List.iter(folder =>
       describe(folder, ({test, _}) =>
         lsDir("./formatTest/" ++ folder)
         |> List.filter(isSourcefile)
         |> List.iter(filename => {
              test(
                filename,
                ({expect}) => {
                  let refmt =
                    buildRefmtArgs(filename, Filename.extension(filename));
                  let (stdOut, stdErr) = syscall(refmt);
                  expect.string(stdOut).toMatchSnapshot();
                  expect.string(stdErr).toBeEmpty();
                },
              )
            })
       )
     );

  describe("errorTests", ({test, _}) =>
    lsDir("./formatTest/errorTests")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let refmt =
               buildRefmtArgs(filename, Filename.extension(filename));
             let (stdOut, stdErr) = syscall(refmt);
             expect.string(stdErr).toMatchSnapshot();
             expect.string(stdOut).toBeEmpty();
           },
         )
       )
  );

  describe("oprintTests", ({test, _}) =>
    lsDir("./formatTest/oprintTests")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let refmt =
               buildRefmtArgs(filename, Filename.extension(filename));
             let (stdOut, stdErr) = syscall(refmt);
             expect.string(stdOut).toMatchSnapshot();
             expect.string(stdErr).toBeEmpty();
           },
         )
       )
  );

  describe("backport_syntax_tests", ({test, _}) => {
    let filename = "./miscTests/backport_syntax_tests/basic.re";

    test(
      filename,
      ({expect}) => {
        let refmt = buildRefmtArgs(filename, Filename.extension(filename));
        let (stdOut, stdErr) = syscall(refmt);
        expect.string(stdOut).toMatchSnapshot();
        expect.string(stdErr).toBeEmpty();
      },
    );
  });

  describe("reactjs_jsx_ppx_tests", ({test, _}) =>
    lsDir("./miscTests/reactjs_jsx_ppx_tests")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let refmt =
               buildRefmtArgs(filename, Filename.extension(filename));
             let (stdOut, stdErr) = syscall(refmt);
             expect.string(stdOut).toMatchSnapshot();
             expect.string(stdErr).toBeEmpty();
           },
         )
       )
  );

  describeSkip("features OCaml 4.08", ({test, _}) =>
    lsDir("./formatTest/features4.08")
    |> List.filter(isSourcefile)
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let ocamlc =
               buildOcamlCompiler(filename, Filename.extension(filename));
             let (stdOut, stdErr) = syscall(ocamlc);
             expect.string(stdOut).toMatchSnapshot();
             expect.string(stdErr).toBeEmpty();
           },
         )
       )
  );
});
