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


ghcid: clean
	ghcid \
		--test="main" \
		--command="ghci" \
		app/Main

ghci:
	ghci app/exe

etags:
	hasktags  -e ./src

clean:
	rm -fR dist dist-*
	find . -name '*.hi' -type f -delete
	find . -name '*.o' -type f -delete
	find . -name '*.dyn_hi' -type f -delete
	find . -name '*.dyn_o' -type f -delete
	find . -name 'autogen*' -type f -delete
