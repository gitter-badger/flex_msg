ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

.PHONY: offline compile get-deps update-deps test clean deep-clean

offline:
	@./rebar compile

compile: get-deps update-deps
	@./rebar compile

beams:
	@./rebar compile

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

test: offline
	@./rebar skip_deps=true eunit

clean:
	@./rebar clean

deep-clean: clean
	@./rebar delete-deps

setup_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools crypto tools inets ssl webtool public_key observer
	dialyzer --add_to_plt deps/*/ebin

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./apps/*/ebin
