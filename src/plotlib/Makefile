# Makefile for the MRAO POSTMORTEM PLOTLIB library
# 26 Jan 99; 10 July 2003

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\

OBJECT_FILES=\
	plot_begin.o plot_complex.o plot_data.o \
	plot_enqmode.o plot_getopts.o plot_set_fit.o \
	plot_setmode.o plot_setpage.o plot_setscale.o \
	plot_setvp.o plot_setzeros.o plot_vstats.o \
	plot_wrerr.o pmadvance.o plot_complexN.o

# Dependencies for building the object library

plotlib.a : $(OBJECT_FILES)
	ar ru plotlib.a $(OBJECT_FILES)

# Target for building the library

build : plotlib.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv plotlib.a ../libplot.a

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > plotlib.for

# Target for splitting library into modules

split :
	fsplit plotlib.for

# Include file dependencies:

