cachix-push:
	nix build --json \
		| jq -r '.[].outputs | to_entries[].value' \
		| cachix push jappie

migration: ## Generate timestamped database migration boilerplate files
	@if test -z "$$name"; then \
	  echo "Usage: make migration name=some-name"; \
	else \
	  migName="`date -u '+%Y%m%d_%H%M%S'`-$$name"; \
	  fname="migrations/up/$$migName.sql"; \
	  fnameDown="migrations/down/$$migName.down.sql"; \
	  touch "$$fname"; \
	  echo "Touched $$fname";\
	  printf "begin;\n\n--YOUR CODE HERE\n\ndelete from schema_migrations where filename = '$$migName.sql';\n\ncommit;\n" >> "$$fnameDown";\
	  echo "Touched $$fnameDown";\
	fi
