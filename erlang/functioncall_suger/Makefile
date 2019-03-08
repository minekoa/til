ERLC=erlc
ERL=erl
ESCRIPT=escript

SRCDIR=src
OUTDIR=ebin
TESTDIR=test

OBJS=mystack.beam \
     mystack2.beam \
    $(NULL)

OBJS4TST= \
    $(NULL)

TESTOBJS= \
    $(NULL)


TEST_NAMES=$(basename $(TESTOBJS))

ifdef TESTS
    TEST_COMMA_LIST=$(TESTS)
else
    TEST_COMMA_LIST=$(shell echo $(TEST_NAMES) | sed -e 's/[ \f\n\r\t]\+/, /g')
endif

.PHONY: all compile run test doc clean

all: compile

compile: $(OBJS:%=$(OUTDIR)/%)

$(OUTDIR)/%.beam: $(SRCDIR)/%.erl | $(OUTDIR)
	$(ERLC) -o $(OUTDIR) $<

$(OUTDIR):
	mkdir $@

test: compile testcompile
	$(ERL) -noshell -pa $(OUTDIR) $(TESTDIR) -eval "eunit:test([$(TEST_COMMA_LIST)], [verbose])" -s init stop

testcompile: $(TESTOBJS:%=$(TESTDIR)/%) $(OBJS4TST:%=$(TESTDIR)/%)

$(TESTDIR)/%.beam: $(TESTDIR)/%.erl
	$(ERLC) -o $(TESTDIR) $<

run:
	$(ESCRIPT) script/run.erl
