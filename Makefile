# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build

install:
	opam pin add reason . -y
	opam pin add -y rtop .
	opam install -y --deps-only ./reason-dev.opam
	opam install -y dune menhir utop
	opam pin add -y pastel https://github.com/reasonml/reason-native.git
	opam pin add -y cli https://github.com/reasonml/reason-native.git
	opam pin add -y file-context-printer https://github.com/reasonml/reason-native.git
	opam pin add -y rely https://github.com/reasonml/reason-native.git

build:
	dune build

# CI uses opam. Regular workflow needn't.
test-ci: tests test-integration

tests:
	dune exec test/Run.exe

test-integration:
	./test/rtopIntegrationTest.sh

.PHONY: coverage
coverage:
	find -iname "bisect*.out" -exec rm {} \;
	make test-integration
	bisect-ppx-report -ignore-missing-files -I _build/ -html coverage-after/ bisect*.out ./*/*/*/bisect*.out
	find -iname "bisect*.out" -exec rm {} \;

all_errors:
	@ echo "Regenerate all the possible error states for Menhir."
	@ echo "Warning: This will take a while and use a lot of CPU and memory."
	@ echo "---"
	menhir --explain --strict --unused-tokens src/reason-parser/reason_parser.mly --list-errors > src/reason-parser/reason_parser.messages.checked-in

clean:
	dune clean

clean-for-ci:
	rm -rf ./_build

.PHONY: build clean

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

# For publishing esy releases to npm
esy-prepublish: build pre_release
	node ./scripts/esy-prepublish.js

# For OPAM
pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/src/refmt/package.ml.in

.PHONY: pre_release

# For OPAM
release_check:
	./scripts/release-check.sh

# For OPAM
release: release_check pre_release
	git add package.json src/refmt/package.ml reason.opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:facebook/Reason.git"
	git push "git@github.com:facebook/Reason.git" tag $(version)
	git clean -fdx
	./scripts/opam-release.sh

.PHONY: release

all-supported-ocaml-versions:
# the --dev flag has been omitted here but should be re-introduced eventually
	dune build @install @runtest --root .

.PHONY: all-supported-ocaml-versions
