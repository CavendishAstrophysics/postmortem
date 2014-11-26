# Makefile for the MRAO POSTMORTEM ION system subroutines
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/post/include/ion_commands.inc \
	/mrao/post/include/ion_definition.inc \
	/mrao/post/include/ion_runtime.inc \
	/mrao/post/include/ionsys_errors.inc

OBJECT_FILES=\
	ion_add_source.o ion_calc_corr.o ion_calc_type1.o \
	ion_calc_type2.o ion_calc_type3.o ion_change_params.o \
	ion_del_source.o ion_disp_corr.o ion_disp_samp.o \
	ion_disp_spect.o ion_disp_wavefront.o ion_edit_corr.o \
	ion_init_ion.o ion_list_corrs.o ion_make_corr.o \
	ion_open.o ion_print.o ion_save.o \
	ion_transform.o ion_wrerr.o

# Dependencies for building the object library

ionsys.a : $(OBJECT_FILES)
	ar ru ionsys.a $(OBJECT_FILES)

# Target for building the library

build : ionsys.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv ionsys.a ..

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > ionsys.for

# Target for splitting library into modules

split :
	fsplit ionsys.for

# Include file dependencies:

