ELM=elm
ELM_FORMAT=elm-format

TARGET=main.js
ELMSRC=src/Main.elm


all: compile

compile: $(TARGET)

$(TARGET): $(ELMSRC)
	$(ELM) make src/Main.elm --output=$(TARGET)

format:
	$(ELM_FORMAT) $(ELMSRC) --yes
