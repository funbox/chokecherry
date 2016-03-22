.PHONY: all compile run test clean
.PHONY: docker_build docker_shell

REBAR=./rebar3

all: compile

compile:
		@$(REBAR) compile

clean:
		@$(REBAR) clean
		rm -rf log

run:
		erl -pa ./_build/default/lib/*/ebin -boot start_sasl -config sys.config -s sync -s lager -s chokecherry

test:
		@$(REBAR) eunit skip_deps=true

docker_build:
		docker build -t chokecherry .

docker_shell:
		docker run -it --rm -v "$(CURDIR)":/mylib -w /mylib chokecherry /bin/bash
