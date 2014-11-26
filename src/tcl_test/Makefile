# Makefile for Postmortem and related programs
#
# Copyright Paul Alexander, MRAO Cambridge
#
# Makefile for Version X0.51 of ANMAP
#
# last updated 10/12/93

# parameters for F77 
FFLAGC=-u
FCOMPL=f77

# parameters for cc
CCOMPL=cc
CFLAGC=-O -DTK_NO_SECURITY

# parameters for TCl/TK and X11
TCL_ROOT = /soft/tcl/v7.3
TCL_DIR = $(TCL_ROOT)/tcl
TK_DIR = $(TCL_ROOT)/tk
TCL_INCDIR = -I$(TK_DIR) -I$(TCL_DIR) -I../include -I$(TCL_ROOT)/TkSteal

TCL_LINK = -L/soft/tcl/v7.3/lib -ltk -ltcl -ltkXAccess -ltclRawTCP -lXpm
X11_LINK = -L/usr/lib/X11 -lX11 -lsocket -lnsl
C_LINK = -lm

# defaults compile for F77 files
.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

# default compile for CC files
.c.o:
	$(CCOMPL) $(CFLAGC) $(TCL_INCDIR) -c $<

OBJECT_FILES=\
	Post.o PostInit.o tcltk_Init.o \
	post_dispatch.o post_init.o block_data.o\
	calib_system.o lsf_system.o lsfun1.o ion_system.o \
	monitor_system.o mon_sys_clfst.o mon_sys_ryle.o \
        mapping_system.o remove_system.o flag_system.o

XOBJECT_FILES=\
	Xpost.o XpostInit.o tcltk_Init.o \
	post_dispatch.o post_init.o block_data.o\
	calib_system.o lsf_system.o lsfun1.o ion_system.o \
	monitor_system.o mon_sys_clfst.o mon_sys_ryle.o \
        mapping_system.o remove_system.o

LIBRARY_FILES=\
	../calsys.a \
        ../ionsys.a \
        ../lsfsys.a \
        ../mapsys.a \
	../remsys.a 

POST_LINK = -L/mrao/lib -L/usr/local/lib -L/usr/lib/X11 -L/mrao/anmap_v7.5/lib\
	-lmon -llsf -lplot -lpg -lsamp -lplanet -lsla -lmap -lfft \
	-lAnmap -lcmd -lutil -lio -lch -lpg -lpgplot -lnag -lAnmap

# object for linking postmortem
postmortem : $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o postmortem \
	$(OBJECT_FILES)  $(LIBRARY_FILES) \
	$(POST_LINK) $(TCL_LINK) $(X11_LINK) $(C_LINK)

# object for linking postmortem
Xpostmortem : $(XOBJECT_FILES) $(LIBRARY_FILES)
	f77 -o Xpostmortem \
	$(XOBJECT_FILES)  $(LIBRARY_FILES) \
	$(POST_LINK) $(TCL_LINK) $(X11_LINK) $(C_LINK)


















