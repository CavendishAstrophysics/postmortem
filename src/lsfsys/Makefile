# Makefile for the MRAO POSTMORTEM LSF system routines
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/post/include/lsfun1.inc

OBJECT_FILES=\
	display_cont_fft.o display_fft.o display_grey.o \
	display_hist.o display_noise.o display_sample.o \
	display_spacing.o display_subband.o fit_spacings.o \
	grey_merge.o \
	lsf_archive_flux.o lsf_enq_adc.o lsf_fit_adc.o lsf_load_common.o \
	merge_spacings.o \
        phase_fit.o print_spacings.o scan_interference.o \
	scan_sample_clfst.o scan_sample_ryle.o scan_samples.o

# Dependencies for building the subroutine library

lsfsys.a : $(OBJECT_FILES)
	ar ru lsfsys.a $(OBJECT_FILES)

# Target for building the library

build : lsfsys.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv lsfsys.a ..

# Target for building single-source module

combine :
	cat *.f > lsfsys.for

# Target for splitting library into modules

split :
	fsplit lsfsys.for

