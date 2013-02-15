ERL ?= erl
APP := vht_node_ptk5
NODE := $(APP)
APPPATH := rel/$(NODE)/bin/$(NODE)
APPPATHWINDOWS := rel\$(NODE)\bin\$(NODE)
REBAR = rebar

include version.properties

all: clean update_deps compile generate installer

init: get_deps compile generate

continuous_integration_full_build: distclean clean get_deps compile lock-deps nsis

.PHONY: nsis generate compile update_deps get_deps clean distclean docs lock-deps upgrade start stop console install_windows uninstall_windows start_windows stop_windows console_windows

compile:
	$(REBAR) compile

generate: rel/$(APP)

rel/$(APP): compile
	$(REBAR) generate

upgrade:
	test ! -d rel/$(NODE)_$(PREVIOUS_VERSION) || $(REBAR) generate-appups previous_release=$(NODE)_$(PREVIOUS_VERSION)
	test ! -d rel/$(NODE)_$(PREVIOUS_VERSION) || $(REBAR) generate-upgrade previous_release=$(NODE)_$(PREVIOUS_VERSION)

nsis: installer/setup.exe

installer/setup.exe: installer/setup.nsi rel/$(APP) installer/*.nsh upgrade
	makensis.exe $<

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

start:
	$(APPPATH) start

stop:
	$(APPPATH) stop

console:
	$(APPPATH) console

install_windows:
	$(APPPATHWINDOWS).cmd install

start_windows:
	$(APPPATHWINDOWS).cmd start

console_windows:
	$(APPPATHWINDOWS).cmd console

stop_windows:
	$(APPPATHWINDOWS).cmd stop

uninstall_windows:
	$(APPPATHWINDOWS).cmd uninstall
