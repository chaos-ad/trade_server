all: get-deps compile

compile:
	./rebar compile

app:
	./rebar compile skip_deps=true

get-deps:
	./rebar get-deps

database:
	./create-db.sh

clean:
	./rebar clean
	rm -rfv erl_crash.dump

distclean: clean
	rm -rfv ebin deps logs database
