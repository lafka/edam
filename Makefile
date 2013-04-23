ERL=erl
ERLC=erlc
APP_NAME=epm
NODE_NAME=epm
VSN=0.1

all: $(wildcard src/*.erl)
	@mkdir -p ebin
	$(ERL) -I lib/*/include -make

ebin/%.app: src/%.app.src
	cp $< $@

doc:
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv .eunit/*.beam
	rm -fv erl_crash.dump

run:
	$(ERL) -pa `pwd`/ebin \
	-boot start_sasl
	-sname $(NODE_NAME)

test/%.erl:
	$(ERL) -D TEST -noshell -pa ebin -eval "eunit:test($(shell echo $@ | sed -E 's/.*\/(.*).erl/\1/'), [verbose])" -s init stop
	
test: $(wildcard src/*.erl)
	$(ERL) erl -D TEST -noshell -pa ebin -eval \
	"eunit:test([$(shell echo $(wildcard src/*.erl) | sed 's/src\///g; s/.erl/,/g;s/,$$//')] \
	, [verbose])" -s init stop
