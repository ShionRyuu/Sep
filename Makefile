.PHONY: compile test clean

REBAR=$(PWD)/rebar

all: compile

compile:
    $(REBAR) compile
    
test: compile
	$(REBAR) eunit   
    
clean:
	$(REBAR) clean
