.PHONY: all compile run test clean
.PHONY: docker_build docker_shell

REBAR=./rebar3

all: compile

compile:
				$(REBAR) compile

run:
				erl -pa _build/default/lib/*/ebin -config sys.config -boot start_sasl -s sync -s chokecherry

test:
				$(REBAR) eunit skip_deps=true verbose=3
				$(REBAR) ct skip_deps=true verbose=3

clean:
				$(REBAR) clean
				rm -rf ./log
				rm -rf ./logs
				rm -rf ./test/*.beam
				rm -rf ./test/TEST*.xml
				rm -rf ./erl_crash.dump

docker_build:
				docker build -t chokecherry .

docker_shell:
				docker run -it --rm -v "$(CURDIR)":/mylib -w /mylib chokecherry /bin/bash

