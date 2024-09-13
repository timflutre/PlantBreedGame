.PHONY: data
version=`cat VERSION`
data:
	Rscript ./initialise_data.R

docker-image:
	nix run .\#images.x86_64-linux.latest.copyToDockerDaemon

release:
	# Run tests and build + push the 2 docker images tagged `latest` and `X.X.X`
	# according to the current version
	git fetch
	@if [ "$$(git rev-parse --abbrev-ref HEAD)" != "master" ]; then \
	  echo "Error: You must be on master branch to make a release"; \
		exit 1; \
	fi
	@if ! git diff --quiet; then \
	  echo "Error: You must not have uncommitted changes to make a release"; \
	  exit 1; \
	fi
	@if [ "$$(git rev-parse @)" != "$$(git rev-parse @{u})" ]; then \
		echo "Your branch is not up-to-date with 'origin/master'. Please sync."; \
		exit 1; \
	fi
	nix run .\#unit_tests
	nix run .\#test_ui
	nix run .\#images.x86_64-linux.latest.copyTo -- docker://docker.io/juliendiot/plantbreedgame:latest
	skopeo --insecure-policy copy docker://docker.io/juliendiot/plantbreedgame:latest docker://docker.io/juliendiot/plantbreedgame:$(version)
	git tag "v"$$(cat VERSION)
	git push --tags

dev-release:
	# Run tests and build + push the a docker images tagged `development`
	@if ! git diff --quiet; then \
	  echo "Error: You must not have uncommitted changes to make a dev-release"; \
	  exit 1; \
	fi
	nix run .\#unit_tests
	nix run .\#test_ui
	nix run .\#images.x86_64-linux.latest.copyTo -- docker://docker.io/juliendiot/plantbreedgame:development

