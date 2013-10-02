#-------------------------------------------------------------------
# @author Chris Jimison
# Created : 2013-07-23 17:37:02.545963
#-------------------------------------------------------------------
REBAR='./rebar'
DIALYZER='dialyzer'
RELEASE='./_rel/bin/barrage'

all: deps dev 
deps:
	@$(REBAR) get-deps
dev:
	@$(REBAR) compile -Ddevelop skip_deps=true 
devFull:
	@$(REBAR) compile -Ddevelop
std:
	@$(REBAR) compile skip_deps=true 
stdFull:
	@$(REBAR) compile
andRun: std
	@$(RELEASE) console
ct: std
	@$(REBAR) ct skip_deps=true verbose=1
ctFull: stdFull
	@$(REBAR) ct skip_deps=true verbose=1
eunit:
	@$(REBAR) skip_deps=true eunit
plt:
	@$(DIALYZER) --build_plt --output_plt plts --apps kernel stdlib runtime_tools mnesia compiler erts hipe edoc gs syntax_tools xmerl crypto asn1 public_key ssl inets ./deps/*/ebin
checkType: dev
	@$(DIALYZER) ./ebin --plt plts -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs -Wno_undefined_callbacks
clean:
	@$(REBAR) clean
