KIT := s60_30
REL := udeb

staticlib :
	cd group && in-gnupoc-env $(KIT) abld-output-filter bldmake bldfiles
	cd group && in-gnupoc-env $(KIT) abld-output-filter abld build gcce $(REL)

all :
	$(MAKE) staticlib KIT=s60_30 REL=udeb
	$(MAKE) staticlib KIT=s60_30 REL=urel
	$(MAKE) staticlib KIT=s60_31 REL=udeb
	$(MAKE) staticlib KIT=s60_31 REL=urel
	$(MAKE) staticlib KIT=s60_32 REL=udeb
	$(MAKE) staticlib KIT=s60_32 REL=urel
	$(MAKE) staticlib KIT=s60_50 REL=udeb
	$(MAKE) staticlib KIT=s60_50 REL=urel
