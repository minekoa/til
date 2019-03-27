ELM=elm
ELM_FORMAT=elm-format

TARGET=index.html
ELMSRC=src/Main.elm


all: compile

compile: $(TARGET)

$(TARGET): $(ELMSRC)
	$(ELM) make src/Main.elm

format:
	$(ELM_FORMAT) $(ELMSRC) --yes
