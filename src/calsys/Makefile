# Makefile for the MRAO POSTMORTEM CALIB library
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\
	/mrao/post/include/cal_ae_matrix.inc \
	/mrao/post/include/cal_common.inc \
	/mrao/post/include/cal_control.inc \
	/mrao/post/include/cal_record.inc \
	/mrao/post/include/cal_solution.inc

OBJECT_FILES=\
	cal_ae_matrix.o cal_calc_soln2.o cal_closure.o \
	cal_disp_sample.o cal_disp_spacing.o cal_display.o \
	cal_fact_type1.o cal_fact_type2.o cal_fact_type3.o \
	cal_fiddle_phase.o cal_gt_apply.o cal_gt_delete.o \
	cal_gt_display.o cal_gt_examine.o cal_gt_excheck.o \
	cal_gt_init.o cal_gt_noamp.o cal_gt_nophi.o \
	cal_gt_update.o cal_gt_write.o cal_gtvis_display.o \
	cal_init_calib.o cal_init_soln.o cal_make_calib.o \
	cal_make_type.o cal_new_geom.o cal_open.o cal_open_lsf.o \
	cal_save.o \
	cal_sel_ampsol.o cal_sel_bandwid.o cal_sel_calib.o \
	cal_sel_model.o cal_sel_phisol.o cal_sel_refant.o \
	cal_sel_vissol.o cal_set_logical.o cal_soln_monitor.o \
	cal_soln_report.o cal_wrerr.o

# Dependencies for building the object library

calsys.a : $(OBJECT_FILES)
	ar ru calsys.a $(OBJECT_FILES)

# Target for building the library

build : calsys.a $(INCLUDE_FILES)

# Target for installing the library

install : calsys.a
	mv calsys.a ..

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > calib.for

# Target for splitting library into modules

split :
	fsplit calib.for

# Include file dependencies:

