##
# UltraStar bamboo build for linux
##

# lazarus base-dir
# will be set by configure later on
LAZDIR := $(shell whereis lazarus | cut -d ' ' -f2)
#LAZDIR = /usr/local/share/lazarus

# Directories added to the unit path
UNIT_FLAG = -Fu
UNIT_PATH = $(UNIT_FLAG)$(LAZDIR)/lcl/units/i386-linux             \
            $(UNIT_FLAG)$(LAZDIR)/components/images/lib/i386-linux \
            $(UNIT_FLAG)$(LAZDIR)/components/jpeg                  \
            $(UNIT_FLAG).
#           $(UNIT_FLAG)$(LAZDIR)/packager/units/i386-linux        \

# Directory where compiled units (.ppu and .o) files are stored
CUNIT_DIR = build/linux/lazarus

# Directories added to the includes path
INC_FLAG = -Fi
INC_PATH = $(INC_FLAG)lib/JEDI-SDLv1.0/SDL/Pas

# Defines
DEFINES  = -dLCL
DEFINES += -dFPC_V220

# Misc fpc options
COMPAT_FLAGS    = -S2dgi
#COMPAT_FLAGS  += -Mdelphi
VERBOSE_FLAGS   = -vew -l
DEBUG_FLAGS     = -gl
DEBUG_FLAGS    += -g
#DEBUG_FLAGS     = -dDEBUG
#-Crtoi
#OPTIMIZE_FLAGS_FPC204 = -OG2p3 
#OPTIMIZE_FLAGS_FPC220 = -O2p"NAME"
#OPTIMIZE_FLAGS  = -dRELEASE 
OPTIMIZE_FLAGS += -Xs
FLAGS = $(COMPAT_FLAGS)    \
        $(VERBOSE_FLAGS)   \
        $(DEBUG_FLAGS)       \
	$(OPTIMIZE_FLAGS)    \
	$(DEFINES) 

# lpr project file used as input
PROJECT_FILE = UltraStar.lpr
# name of executable
OUTPUT = UltraStar

all: resource ultrastar
resource: UltraStar.lrs
ultrastar: clean_obj
	mkdir -p $(CUNIT_DIR)
	fpc $(FLAGS) $(INC_PATH) $(UNIT_PATH) -FU$(CUNIT_DIR) -o$(OUTPUT) $(PROJECT_FILE)
install:
	mv $(OUTPUT) ../..
clean: clean_obj clean_res
distclean: clean
clean_obj:
	rm -f -r $(CUNIT_DIR)/*.o $(CUNIT_DIR)/*.ppu $(CUNIT_DIR)/*.rst
	rm -f $(OUTPUT)
clean_res:
	rm -f UltraStar.lrs link.res
UltraStar.lrs: UltraStar.rc
	sh lazres-UltraStar.sh

.PHONY: all resource ultrastar install clean distclean clean_obj clean_res
