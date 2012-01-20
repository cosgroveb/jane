ERL ?= erl
APP := jane

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
	@./rebar eunit app=jane

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

rel: deps
	@./rebar compile generate

