VERSION := $(shell cat VERSION)

build-docker:
	docker build . --tag starlane/starlane:${VERSION}
	docker build . --tag starlane/starlane-k8s:${VERSION} --build-arg FEATURES=k8s

push: build-docker
	docker push starlane/starlane:${VERSION}
	docker push starlane/starlane-k8s:${VERSION}

build-ctrl:
	$(MAKE) -C kubernetes/ctrl

build: build-docker build-ctrl

kube-install-operator:
	cd go/starlane-operator && ./build.sh && ./deploy.sh

kube-install-basics:
	$(MAKE) -C kubernetes/basics

kube-install-starlane: kube-install-operator
	kubectl apply -f kubernetes/starlane.yaml

kube-install: kube-install-starlane kube-install-basics 

the-docs:
	cd docs && hugo -D 
	skaffold -f skaffold-docs.yaml run 


install: 
	cd rust/starlane && cargo install --path .


