REBAR3=rebar3

.PHONY: all eescriptize dialyzer run


all:
	$(REBAR3) escriptize
	$(REBAR3) dialyzer

escriptize:
	$(REBAR3) escriptize

dialyzer:
	$(REBAR3) dialyzer

run: all
	./_build/default/bin/hello_erlando

