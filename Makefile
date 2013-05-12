REBAR := ./rebar

.PHONY: all erl test clean doc

all: erl

erl:
	$(REBAR) get-deps compile

test: erl
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit

doc: erl
	$(REBAR) doc
