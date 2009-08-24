CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

SCRIPT := create-megasis.rb
CREATE := ruby $(SCRIPT)
FLAGS :=             # you may pass FLAGS=--nr
BASENAME := cl2_megasis
TEMPLATE := template.pkg.in
PKGFILE := $(BASENAME)-$(DIST_VARIANT_NAME).pkg
SISFILE := $(BASENAME)-$(VERSION_STRING)-$(DIST_VARIANT_NAME).sis
SISXFILE := $(SISFILE)x

default : $(if $(SIGNED), $(SISXFILE), $(SISFILE))

$(PKGFILE) : $(TEMPLATE) $(SCRIPT) $(CURRENT_CONFIG)
	$(CREATE) $(FLAGS) -o $(PKGFILE) $(TEMPLATE)

$(SISXFILE) : $(PKGFILE)
	in-gnupoc-env s60_30 do-make-sign-sis --cert $(CERT_NAME) --makesis -o $@ $<

$(SISFILE) : $(PKGFILE)
	in-gnupoc-env s60_30 do-make-sign-sis --unsigned --makesis -o $@ $<

