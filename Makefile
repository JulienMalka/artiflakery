BIN_DIR=$(shell stack path --local-install-root)/bin

build:
	stack build

run: build
	exec $(BIN_DIR)/artiflakery-exe
