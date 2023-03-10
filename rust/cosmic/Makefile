version:
	$(MAKE) -C cosmic-nom version
	$(MAKE) -C cosmic-config version
	$(MAKE) -C cosmic-macros-primitive version
	$(MAKE) -C cosmic-space version
	$(MAKE) -C cosmic-macros version
	$(MAKE) -C cosmic-hyperlane version
	$(MAKE) -C cosmic-hyperlane-tcp version
	$(MAKE) -C cosmic-hyperspace version
	$(MAKE) -C cosmic-cli version
	$(MAKE) -C cosmic-registry-postgres version
	$(MAKE) -C ../mechtron/mechtron-host version
	$(MAKE) -C ../mechtron/mechtron version

publish-dry-run:
	$(MAKE) -C cosmic-nom publish-dry-run
	$(MAKE) -C cosmic-config publish-dry-run
	$(MAKE) -C cosmic-macros-primitive publish-dry-run
	$(MAKE) -C cosmic-space publish-dry-run
	$(MAKE) -C cosmic-macros publish-dry-run
	$(MAKE) -C cosmic-hyperlane publish-dry-run
	$(MAKE) -C cosmic-hyperlane-tcp publish-dry-run
	$(MAKE) -C cosmic-hyperspace publish-dry-run
	$(MAKE) -C cosmic-cli publish-dry-run
	$(MAKE) -C cosmic-registry-postgres publish-dry-run
	$(MAKE) -C ../mechtron/mechtron-host publish-dry-run

publish:
	$(MAKE) -C cosmic-nom publish
	$(MAKE) -C cosmic-config publish
	$(MAKE) -C cosmic-macros-primitive publish
	sleep 30 
	$(MAKE) -C cosmic-space publish
	sleep 30 
	$(MAKE) -C cosmic-macros publish
	sleep 30 
	$(MAKE) -C cosmic-hyperlane publish
	sleep 30 
	$(MAKE) -C cosmic-hyperlane-tcp publish
	sleep 30
	$(MAKE) -C ../mechtron/mechtron-host publish
	$(MAKE) -C ../mechtron/mechtron publish
	$(MAKE) -C cosmic-hyperspace publish
	$(MAKE) -C cosmic-cli publish
	$(MAKE) -C cosmic-registry-postgres publish

 
