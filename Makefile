all: get-deps compile

compile:
	./rebar compile


get-deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rfv erl_crash.dump

distclean: clean
	rm -rfv ebin deps
