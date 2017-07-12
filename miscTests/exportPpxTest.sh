#!/bin/bash

set +e
testPath="miscTests/export_tests"

failures=0

only=$1

if [ -z $only ]; then

  for name in $testPath/*.ml
  do
    base=$(basename "$name")
    base="${base%.ml}"
    echo ">> Checking ${base}"
    ocamlc -dsource -ppx ./ppx_export.native $name -o test.native &> ${name}.actual
    if [[ "$?" = "0" ]]; then
      echo "OK"
    else
      failures=$((failures+1))
      cat ${name}.actual
      rm ${name}.actual
    fi
    rm -rf miscTests/export_tests/${base}.cm*
  done

  # Expected failures
  for name in $testPath/invalid/*.ml
  do
    base=$(basename "$name")
    base="${base%.ml}"
    echo ">> Expect failure for ${base}"
    ocamlc -dsource -ppx ./ppx_export.native $name -o test.native 2> ${name}.err > ${name}.actual
    if [[ "$?" = "0" ]]; then
      failures=$((failures+1))
      echo "Expected failure?"
      cat ${name}.err
      cat ${name}.actual
    else
      echo "OK"
    fi
    rm ${name}.err
    rm -rf miscTests/export_tests/invalid/${base}.cm*
  done

  if [[ "$failures" -gt 0 ]]; then
    echo "${failures} tests failed"
    exit 1
  else
    echo "All tests passing"
    exit 0
  fi

else
  base=$only
  name=$testPath/${only}.ml
  echo ">> Checking ${base}"
  ocamlc -dsource -ppx ./ppx_export.native $name -o test.native &> ${name}.actual
  if [[ "$?" = "0" ]]; then
    echo "OK"
  else
    failures=$((failures+1))
    cat ${name}.actual
    rm ${name}.actual
  fi
  rm -rf miscTests/export_tests/${base}.cm*

fi

