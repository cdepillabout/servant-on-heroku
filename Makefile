.PHONY: build clean docker-build ghci heroku-release hlint lint run watch
all: build

# Build this package.
build:
	stack build servant-on-heroku

# Build a docker image.
docker-build:
	docker build -t servant-on-heroku .

# Perform a `stack clean`.
clean:
	stack clean

# Run a GHCi REPL with stack.
ghci:
	stack ghci

# Push the current version of the app to heroku.
heroku-release:
	heroku container:push web

# Perform linting with hlint.
hlint: lint
lint:
	hlint src/

# Build and run the app locally.
run: build
	stack exec -- servant-on-heroku-api

# Rebuild the app while watching for changes.
watch:
	stack build --file-watch --fast servant-on-heroku
