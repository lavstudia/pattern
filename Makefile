.PHONY: compile clean doc

REBAR=./rebar

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc
