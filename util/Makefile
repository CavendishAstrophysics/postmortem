# Makefile for the data conversion utilities
#
# DJT, 28/2/94

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

LIBRARY_FILES=\
        /mrao/lib/libsamp.a

all : sf lsf ion gt

sf : convert_sf.o $(LIBRARY_FILES)
	f77 -o convert_sf convert_sf.o \
	-L/mrao/lib -lsamp -lsla -lutil -lio -lch

lsf : convert_lsf.o $(LIBRARY_FILES)
	f77 -o convert_lsf convert_lsf.o \
	-L/mrao/lib -lsamp -lsla -lutil -lio -lch

ion : convert_ion.o $(LIBRARY_FILES)
	f77 -o convert_ion convert_ion.o \
	-L/mrao/lib -lsamp -lsla -lutil -lio -lch

gt : convert_gt.o convert_gtvis.o
	f77 -o convert_gt convert_gt.o -L/mrao/lib -lutil -lio -lch
	f77 -o convert_gtvis convert_gtvis.o -L/mrao/lib -lutil -lio -lch

