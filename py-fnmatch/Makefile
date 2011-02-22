CERT := dev
KIT := s60_30
DEVICE := default

SIGN := ../megasis/do-sis-signing.rb 
SISX_FILE := fnmatch-pys60v1_$(CERT).sisx

all :
	$(MAKE) sis CERT=dev
	$(MAKE) sis CERT=self

sis :
	$(SIGN) -k $(KIT) --makesis --signsis -c $(CERT) -i pys60v1-fnmatch.pkg -o $(SISX_FILE)

-include local.mk
