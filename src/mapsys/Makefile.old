# Makefile for the MRAO POSTMORTEM mapping system subroutines
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/post/include/mapsys_save.inc

OBJECT_FILES=\
	buffer_wt.o correct_map.o exm_print.o \
	get_skew_angle.o get_smooth_array.o get_weight_buff.o \
	grade_aperture.o grid_vis_buff.o map_add_lsf.o \
	map_chredt.o map_delete_lsf.o map_init_redtape.o \
	map_plot.o map_save.o map_sel_bset.o map_sel_conv.o \
	map_sel_gr_cent.o map_sel_grading.o map_sel_proj.o \
	map_sel_size.o map_sel_type.o map_sel_uv_cent.o \
	map_sel_weight.o map_wrerr.o mapper.o write_aperture.o

# Dependencies for building the object library

mapsys.a : $(OBJECT_FILES)
	ar ru mapsys.a $(OBJECT_FILES)

# Target for building the library

build : mapsys.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv mapsys.a ..

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > mapsys.for

# Target for splitting library into modules

split :
	fsplit mapsys.for

# Include file dependencies:

