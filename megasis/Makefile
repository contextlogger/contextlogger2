configure = ./configure.scm $(1)
configure = make config VARIANT_NAME=$(1)

default : s60_30_dev

s60_% :
	cd ../daemon && $(call configure,$@)
	$(MAKE) -f build.mk static_libs
	$(MAKE) -f build.mk $@
	$(MAKE) -f megasis.mk

release :
	$(MAKE) -f release.mk

clean :
	-rm *~ *.pkg *.sis *.sisx

