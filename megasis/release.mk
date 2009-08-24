CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

DIST_HOME := ../download

release :
	mkdir -p $(DIST_HOME)
	$(MAKE) s60_30_self30 
	$(MAKE) s60_30_self32
	$(MAKE) s60_30_unsigned
	cp cl2_megasis-$(VERSION_STRING)-s60_30_self30.sisx $(DIST_HOME)/
	cp cl2_megasis-$(VERSION_STRING)-s60_30_self32.sisx $(DIST_HOME)/
	cp cl2_megasis-$(VERSION_STRING)-s60_30_dev.sis $(DIST_HOME)/
