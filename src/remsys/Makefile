# Makefile for the MRAO POSTMORTEM REMOVE system routines
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/post/include/remove_common.inc \
	/mrao/post/include/remove_record.inc

OBJECT_FILES=\
	rem_display.o rem_init_remove.o rem_make_remove.o \
	rem_open.o rem_save.o rem_sel_model.o \
	rem_wrerr.o

# Dependencies for building the subroutine library

remsys.a : $(OBJECT_FILES)
	ar ru remsys.a $(OBJECT_FILES)

# Target for building the library

build : remsys.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv remsys.a ..

# Target for building single-source module

combine :
	cat *.f > remsys.for

# Target for splitting library into modules

split :
	fsplit remsys.for

