# Makefile for UV2FITS
#
# DJT, 16/2/98

FFLAGC=-u
FCOMPL=f77

BINDIR=/mrao/post/bin

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=uv2fits-common.inc

LIBRARY_FILES=\
        /mrao/lib/liblsf.a \
        /mrao/lib/libmon.a \
        /mrao/lib/libmap.a \
        /mrao/lib/libsamp.a \
	/mrao/lib/libplanet.a

OBJECT_FILES=fits_an2dt.o fits_d2t.o fits_fq2dt.o fits_h2dt.o uv2fits.o

# object for linking uv2fits

uv2fits: $(OBJECT_FILES) $(LIBRARY_FILES)
	f77 -o uv2fits $(OBJECT_FILES) \
	-L/mrao/lib -L/usr/local/lib \
	-lmon -llsf -lsamp -lplanet -lsla -lmap -lutil -lio -lch
	mv uv2fits $(BINDIR)

clean:
	rm *.o

