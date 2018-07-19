# Tests the jsx ppx for ReasonReact. Call `jsxPpxTest.sh update` to update the
# expected results

echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"
tempFile="temp_jsx_test.ml"

WARNING='\033[0;31m'
SUCCESS='\033[0;32m'
RESET='\033[0m'

# for better visual diffing in the terminal, try https://github.com/jeffkaufman/icdiff
if hash icdiff 2>/dev/null; then
  DIFF="icdiff"
else
  DIFF="diff -u"
fi

for version in v2 v3
do
  for i in {1..3}
  do
    test="$testPath/test$i.re"
    expected="$testPath/expected_${version}_${i}.re"
    actual="$testPath/actual_${version}_${i}.re"

    # for each test, we're gonna use ocamlc and the ppx to dump the post-ppx ocaml
    # file somewhere

    ocamlc -dsource -ppx "./_build/default/.ppx/jbuild/reason.reactjs_jsx_ppx_${version}/ppx.exe --as-ppx" \
      -pp "./_build/install/default/bin/refmt --print binary" -impl $test \
      2> $tempFile

    # if there's an Error/Fatal error, bail early
    if grep -q "rror: " $tempFile; then
      printf "${WARNING}V2 behavior $i: error in output after the ppx ran. Here's the content:${RESET}\n"
      cat $tempFile
      rm $tempFile
      exit 1
    fi
    # no error
    ./_build/install/default/bin/refmt --print-width 80 --parse ml --print re $tempFile > $actual

    rm $tempFile

    # if this script's called with the argument `update`, update the expected
    # result instead.
    if [[ $1 = "update" ]]; then
      cat $actual > $expected && echo "\033[0;32mUpdated tests\033[m"
    fi

    if [[ "$(cat $expected)" = "$(cat $actual)" ]]; then
      printf "${SUCCESS}$version behavior $i: ok${RESET}\n"
    else
      printf "${WARNING}$version behavior $i: wrong${RESET}\n"
      # show the error diff
      $DIFF $expected $actual
      exit 1
    fi
  done
done
