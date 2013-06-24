ERL = erl
VSN = 0.1

PROJECT ?= edam
MODULE ?= edm_main

TMPDIR = /tmp/$(PROJECT)-build-$(VSN)

all: compile ebin/$(PROJECT).app script

compile: $(wildcard src/*.erl)
	@mkdir -p ebin
	$(ERL) -I lib/*/include -make

ebin/%.app: src/%.app.src
	cp $< $@

doc:
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(PROJECT)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv .eunit/*.beam
	rm -fv erl_crash.dump

test/%.erl:
	$(ERL) -D TEST -noshell -pa ebin -eval "eunit:test($(shell echo $@ | sed -E 's/.*\/(.*).erl/\1/'), [verbose])" -s init stop

test: $(wildcard src/*.erl)
	$(ERL) erl -D TEST -noshell -pa ebin -eval \
	"eunit:test([$(shell echo $(wildcard src/*.erl) | sed 's/src\///g; s/.erl/,/g;s/,$$//')] \
	, [verbose])" -s init stop

BASEDIR = $(shell basename $(PWD))
script: compile
	@mkdir -p $(TMPDIR)
	@ln -sf $(PWD) $(TMPDIR)/$(PROJECT)
	@cd $(TMPDIR) && \
		zip build.zip $(PROJECT)/ebin/* $(PROJECT)/$(wildcard src/*.erl) > /dev/null && \
		echo '#!/usr/bin/env escript' > target && \
		echo '%%! -escript main $(MODULE)' >> target && \
		cat build.zip >> target && \
		chmod +x target && \
		cp target $(PWD)/$(PROJECT) && \
		rm -r $(TMPDIR)
	@echo Script '$(PROJECT)' is now in your root
