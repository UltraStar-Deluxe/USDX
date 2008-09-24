MKDIR  ?= mkdir -p
RM     ?= rm -f
RM_REC ?= $(RM) -r

PASDOC ?= pasdoc$(EXEEXT)

DOCDIR   ?= ./pasdoc
SRCDIR   := ../src
INCLUDE  := -I$(SRCDIR)
DEFINES  := -DPASDOC
SRCFILES := $(SRCDIR)/base/*.pas \
            $(SRCDIR)/screens/*.pas \
            $(SRCDIR)/menu/*.pas \
			$(SRCDIR)/media/*.pas 

.PHONY: all
all: doc

.PHONY: doc
doc: clean
	$(MKDIR) $(DOCDIR)
# pasdoc does not return a meaningful exit code (e.g. an error code on success) so always return true
	$(PASDOC) --staronly --ignore-leading=* $(INCLUDE) $(DEFINES) --output=$(DOCDIR) $(SRCFILES); true
# check if doc was created
	@test -f $(DOCDIR)/index.html

.PHONY: clean
clean:
	$(RM) $(DOCDIR)/*.html $(DOCDIR)/*.css $(DOCDIR)/*.gif
	-rmdir $(DOCDIR) 