.PHONY: ps erl all run test

all: ps erl

ps:
	psc-package sources | xargs purs compile 'src/**/*.purs'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

run:
	erl -pa ebin -noshell -eval '(main@ps:main())()' -eval 'init:stop()'

ps_test:
	psc-package sources | xargs purs compile 'src/**/*.purs' 'test/**/*.purs'

test: ps_test erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'
