.PHONY: data
version=`cat VERSION`

data:
	Rscript ./initialise_data.R

docker-image:
	nix run .\#images.x86_64-linux.latest.copyToDockerDaemon

release:
	# Run tests and build + push the 2 docker images tagged `latest` and `X.X.X`
	# according to the current version
	nix run .\#unit_tests
	nix run .\#test_ui
	nix run .\#images.x86_64-linux.latest.copyTo -- docker://docker.io/juliendiot/plantbreedgame:latest
	skopeo --insecure-policy copy docker://docker.io/juliendiot/plantbreedgame:latest docker://docker.io/juliendiot/plantbreedgame:$(version)

dev-release:
	# Run tests and build + push the a docker images tagged `development`
	nix run .\#unit_tests
	nix run .\#test_ui
	nix run .\#images.x86_64-linux.latest.copyTo -- docker://docker.io/juliendiot/plantbreedgame:development

