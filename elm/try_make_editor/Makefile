ELM_MAKE=elm-make
ELM_PACKAGE=elm-package
ELM_TEST=elm-test

SRC_DIR=src
DEMO_DIR=demo
OUT_DIR=dist
TARGET=main.js

.PHONY: all env compile release clean

all: $(OUT_DIR)/$(TARGET)
	cp $(DEMO_DIR)/index.html $(OUT_DIR)/index.html


env:
	$(ELM_PACKAGE) install
	mkdir -p $(OUT_DIR)


$(OUT_DIR)/$(TARGET): $(DEMO_DIR)/Main.elm
	$(ELM_MAKE) $(DEMO_DIR)/Main.elm --output $(OUT_DIR)/$(TARGET)

clean:
	rm -rf $(OUT_DIR)

test:
	$(ELM_TEST)
