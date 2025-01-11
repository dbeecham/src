default: minimal-example.nix simple-option.nix apply.nix

.PHONY: minimal-example.nix
minimal-example.nix:
	@echo $@:
	@nix eval --impure --expr "builtins.attrNames (import ./$@)"

.PHONY: simple-option.nix
simple-option.nix:
	@echo $@:
	@nix eval --file $@ config

.PHONY: apply.nix
apply.nix:
	@echo $@:
	@nix eval --file $@ config
