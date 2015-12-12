.PHONY: test-deps test-clean test-compile test compile

test: test-clean test-compile compile
	@erl -shutdown_time 10 -noshell \
	-pa ebin -pa test -pa deps/*/ebin \
	-eval 'bddr_suite:run_suite(rm_cache_SUITE).'

compile:
	@rebar compile

shell: test-deps test-compile
	@erl -pa ebin -pa test -pa deps/*/ebin

test-clean:
	@rm -f ./ebin/*beam; rm -f ./test/*beam

test-compile:
	@erlc -I ./.. -pa ebin -pa test -o test -Werror test/*.erl

test-deps:
	@rebar -C rebar.test.config get-deps
	@rebar -C rebar.test.config compile
