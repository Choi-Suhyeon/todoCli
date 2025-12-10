INPUT_MD = ./docs/long-help.md
OUTPUT_TXT = ./docs/long-help.txt
OUTPUT_MAN = ./docs/todoCli.1

.PHONY: all
all: build-haskell

.PHONY: build-haskell
build-haskell: $(OUTPUT_TXT) $(OUTPUT_MAN)
	stack build

$(OUTPUT_TXT): $(INPUT_MD)
	pandoc -s -t plain $< -o $@

$(OUTPUT_MAN): $(INPUT_MD)
	pandoc -s -t man $< -o $@

.PHONY: install
install: $(OUTPUT_MAN)
	@SYSTEM_NAME=$$(uname); \
	if [ "$$SYSTEM_NAME" = "Linux" ] || [ "$$SYSTEM_NAME" = "Darwin" ]; then \
		MAN_INSTALL_DIR=/usr/local/share/man/man1; \
		sudo mkdir -p $$MAN_INSTALL_DIR; \
		sudo install -m 0644 $< $$MAN_INSTALL_DIR/todoCli.1; \
    fi

.PHONY: clean
clean:
	rm -f $(OUTPUT_TXT) $(OUTPUT_MAN)
	stack clean
