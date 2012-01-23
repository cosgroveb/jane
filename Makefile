ERL ?= erl
APP := jane

.PHONY: deps test

all: deps
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps clean compile

clean:
	@./rebar clean skip_deps=true

distclean: clean
	@./rebar delete-deps

test:
	@./rebar eunit app=jane

run:
	@erl -config test -pa ebin deps/*/ebin -s jane

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

rel: deps
	@./rebar compile generate

