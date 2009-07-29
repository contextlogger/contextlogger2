ZDIR=$(EPOCROOT)epoc32/data/z
TARGETDIR=$(ZDIR)/resource/apps
BASENAME=cl2app
ICONTARGETFILENAME=$(TARGETDIR)/$(BASENAME)_aif.mif

do_nothing :

MAKMAKE : do_nothing

BLD : do_nothing

CLEAN : do_nothing

LIB : do_nothing

CLEANLIB : do_nothing

RESOURCE : 
	mifconv $(ICONTARGETFILENAME) /c32 $(BASENAME).svg

FREEZE : do_nothing

SAVESPACE : do_nothing

RELEASABLES :
	@echo $(ICONTARGETFILENAME)

FINAL : do_nothing
