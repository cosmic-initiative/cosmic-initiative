
.PHONY : clean version
clean :
	find . -type f -name "*.toml" -exec touch {} +
	find . -type f -name "Makefile" -exec touch {} +


version:
	cp ../VERSION cosmic/cosmic-space/src
	$(MAKE) -C cosmic
	$(MAKE) -C mechtron 
	$(MAKE) -C starlane/starlane 
	$(MAKE) -C moogon/moogon

publish-dry-run-impl: 
	rustup default stable
	$(MAKE) -C cosmic publish-dry-run
	$(MAKE) -C starlane/starlane publish-dry-run
	$(MAKE) -C moogon/moogon publish-dry-run


publish-impl:
	rustup default stable
	$(MAKE) -C cosmic publish
	sleep 30
	$(MAKE) -C starlane/starlane publish
	$(MAKE) -C moogon/moogon publish

publish-dry-run: version publish-dry-run-impl
publish: version publish-impl
 
