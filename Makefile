ERL ?= erl
APP := resource_manager
REBAR = rebar

build: compile eunit

init: get_deps compile eunit

continuous_integration_full_build: distclean clean get_deps compile lock-deps eunit

compile:
	$(REBAR) compile

eunit:
	$(REBAR) eunit skip_deps=true

update_deps:
	$(REBAR) update-deps

get_deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean:
	$(REBAR) delete-deps

docs:
	$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

lock-deps:
	$(REBAR) lock-deps