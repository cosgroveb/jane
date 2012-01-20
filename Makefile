ERL ?= erl
APP := jane_web

.PHONY: deps test

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar eunit app=jane,jane_web

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

rel: deps
	@./rebar compile generate

