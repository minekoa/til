ELM_MAKE=elm-make
ELM_PACKAGE=elm-package

SRC_DIR=src
OUT_DIR=dist
TARGET=main.js

.PHONY: all env compile release clean

all: $(OUT_DIR)/$(TARGET)
	cp index.html $(OUT_DIR)/index.html


env:
	$(ELM_PACKAGE) install
	mkdir -p $(OUT_DIR)


$(OUT_DIR)/$(TARGET): $(SRC_DIR)/Main.elm
	$(ELM_MAKE) $(SRC_DIR)/Main.elm --output $(OUT_DIR)/$(TARGET)

clean:
	rm -rf $(OUT_DIR)
