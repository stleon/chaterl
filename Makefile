compile:
	./rebar3 compile

release:
	./rebar3 as prod release -n chat

run:
	./rebar3 run

dev-install:
	cp config/sys.config config/sys.config.devel

tar:
	./rebar3 as prod tar release -n chat