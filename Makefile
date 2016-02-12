.PHONY: all deps compile compile_without_reba run test clean

REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

compile_without_rebar:
	for f in `ls src/*`; do erlc -pa ../lager/ebin/ -o ebin $$f; done

clean:
	rm -rf ebin/* log deps

run:
	erl -pa ./ebin -pa deps/*/ebin -boot start_sasl -config sys.config -s sync -s lager -s chokecherry

test:
	@$(REBAR) eunit skip_deps=true
