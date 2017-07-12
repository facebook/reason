# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build test

setup_convenient_bin_links:
	mkdir -p $(shell pwd)/_build/bin
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/_reasonbuild/_build/myocamlbuild $(shell pwd)/_build/bin/rebuild
	ln -fs $(shell pwd)/_build/src/ocamlmerlin_reason.native $(shell pwd)/_build/bin/ocamlmerlin-reason
	ln -fs $(shell pwd)/_build/src/reason_format_type.native $(shell pwd)/_build/bin/refmttype
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/src/share.sh $(shell pwd)/_build/bin/share
	ln -fs $(shell pwd)/_build/src/reup.sh $(shell pwd)/_build/bin/reup

precompile:
	cp pkg/META.in pkg/META
	ocamlbuild -package topkg pkg/build.native

build_without_utop: setup_convenient_bin_links precompile
	./build.native build --utop false
	chmod +x $(shell pwd)/_build/src/*.sh

build: setup_convenient_bin_links precompile
	./build.native build --utop true
	chmod +x $(shell pwd)/_build/src/*.sh

install:
	opam pin add reason . -y
	./refmt_impl.native --help=groff > $(shell opam config var man)/man1/refmt.1

run: build
	rlwrap ocaml \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        findlib compiler-libs.common unix) \
		_build/src/reason.cma _build/src/reason_toploop.cmo

run_utop: build
	utop \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        compiler-libs.common) \
		_build/src/reason.cma _build/src/reason_utop.cmo

test: build clean-tests
	./miscTests/rtopIntegrationTest.sh
	./miscTests/jsxPpxTest.sh
	./miscTests/exportPpxTest.sh
	cd formatTest; ./test.sh

clean-tests:
	rm -rf ./formatTest/**/actual_output
	rm -f ./formatTest/failed_tests

clean: clean-tests
	ocamlbuild -clean

.PHONY: build clean

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/package.json.in; \
	$(SUBSTS) $(ROOT_DIR)/package.ml.in; \
	$(SUBSTS) $(ROOT_DIR)/opam.in
	# pre_release and make tarball of subpkg
	make -C $(ROOT_DIR)/reason-parser pre_release

.PHONY: pre_release

release_check:
	./scripts/release-check.sh

release: release_check pre_release
	git add package.json package.ml opam reason-parser/package.json reason-parser/opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:facebook/Reason.git"
	git push "git@github.com:facebook/Reason.git" tag $(version)
	git clean -fdx
	./scripts/opam-release.sh

.PHONY: release
