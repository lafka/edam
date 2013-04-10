ERL=erl
APP_NAME=epm
NODE_NAME=epm
VSN=0.1

all: $(wildcard src/*.erl)
	$(ERL) -I lib/*/include -make

ebin/%.app: src/%.app.src
	cp $< $@

doc:
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

run:
	$(ERL) -pa `pwd`/ebin \
	-boot start_sasl
	-sname $(NODE_NAME)
