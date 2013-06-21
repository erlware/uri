# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)
ERL_VER=$(shell erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'  -noshell)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

URI_PLT=$(CURDIR)/.uri_plt.$(ERL_VER)

.PHONY: all compile doc clean test shell distclean pdf get-deps rebuild dialyzer typer

all: compile doc test

deps:
	$(REBAR) get-deps compile

get-deps:
	$(REBAR) get-deps compile

compile: deps
	$(REBAR) skip_deps=true compile

doc: compile
	- $(REBAR) skip_deps=true doc

test: compile
	$(REBAR) skip_deps=true eunit

$(URI_PLT):
	@echo Building local plt at $(URI_PLT)
	@echo
	- dialyzer --fullpath --verbose --output_plt $(URI_PLT) --build_plt \
	   --apps erts kernel stdlib eunit -r deps

dialyzer: test $(URI_PLT)
	dialyzer --fullpath --plt $(URI_PLT) -Wrace_conditions --src -r ./src

typer: $(URI_PLT)
	typer --plt $(URI_PLT) -r ./src

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

clean:
	$(REBAR) skip_deps=true clean
	- rm $(CURDIR)/doc/*.html
	- rm $(CURDIR)/doc/*.css
	- rm $(CURDIR)/doc/*.png
	- rm $(CURDIR)/doc/edoc-info

distclean: clean
	rm -rf $(URI_PLT).$(ERL_VER)
	rm -rvf $(CURDIR)/deps

rebuild: distclean all
