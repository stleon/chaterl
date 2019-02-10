compile:
	./rebar3 compile

release:
	./rebar3 release

run:
	./rebar3 run

dev-install:
	cp config/sys.config config/sys.config.devel
