NAME := hook
ES   := $(wildcard examples/*.eff)
ARGS := batch $(patsubst %.eff,%,$(ES))

.PHONY: run
run:
	stack run -- $(ARGS)

.PHONY: interact
interact: $(NAME).cabal
	stack ghci $(NAME)

.PHONY: build
build: $(NAME).cabal
	stack build

.PHONY: build-profile
build-profile: $(NAME).cabal
	stack build --profile

.PHONY: install
install: $(NAME).cabal
	stack install

.PHONY: configure
configure: $(NAME).cabal

$(NAME).cabal: package.yaml
	hpack --force

.PHONY: tidy
tidy:
	rm examples/*.out

.PHONY: clean
clean:
	stack clean
	rm -f $(NAME).cabal
	$(MAKE) tidy
