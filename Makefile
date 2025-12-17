INPUT_MD = ./docs/long-help.md
OUTPUT_TXT = ./docs/long-help.txt
OUTPUT_MAN = ./docs/todo.1

DHALL_DIR = ./dhall
DHALL_TO_YAML = dhall-to-yaml

FOURMOLU_DHALL = $(DHALL_DIR)/fourmolu.dhall
HIE_DHALL      = $(DHALL_DIR)/hie.dhall
PACKAGE_DHALL  = $(DHALL_DIR)/package.dhall
STACK_DHALL    = $(DHALL_DIR)/stack.dhall

FOURMOLU_YAML = ./fourmolu.yaml
HIE_YAML      = ./hie.yaml
PACKAGE_YAML  = ./package.yaml
STACK_YAML    = ./stack.yaml

.PHONY: all
all: build-haskell

.PHONY: build-haskell
build-haskell: dhall-yaml $(OUTPUT_TXT) $(OUTPUT_MAN)
	stack build

.PHONY: dhall-yaml
dhall-yaml: $(FOURMOLU_YAML) $(HIE_YAML) $(PACKAGE_YAML) $(STACK_YAML)

$(FOURMOLU_YAML): $(FOURMOLU_DHALL)
	$(DHALL_TO_YAML) --file $< --output $@ --preserve-null

$(HIE_YAML): $(HIE_DHALL)
	$(DHALL_TO_YAML) --file $< --output $@ --preserve-null

$(PACKAGE_YAML): $(PACKAGE_DHALL)
	$(DHALL_TO_YAML) --file $< --output $@

$(STACK_YAML): $(STACK_DHALL)
	$(DHALL_TO_YAML) --file $< --output $@

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
		sudo install -m 0644 $< $$MAN_INSTALL_DIR/$$(basename $<); \
    fi

.PHONY: clean
clean:
	rm -f $(OUTPUT_TXT) $(OUTPUT_MAN)
	stack clean
