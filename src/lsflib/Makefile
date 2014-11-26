# Makefile for the MRAO POSTMORTEM LSFLIB library
# 26 Jan 99; 31 Mar 2000

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\

OBJECT_FILES=\
	bandwidth.o bits_to_string.o buffer_num.o calibrate.o \
	close_remcal.o degrid.o enconv.o enposobsdat.o \
	flag_add_entry.o flag_calculate.o flag_close.o \
	flag_copy_entries.o flag_create.o flag_docalc.o \
	flag_enq_entries.o flag_enq_last_version.o flag_err.o \
	flag_get_key.o flag_get_list.o flag_list_entries.o \
	flag_open.o flag_read_entry.o flag_system.o \
	flag_write_entry.o flagging.o get_lsf.o \
	init_lsf_desc.o init_runtime_lsf.o int_chop.o integ_time.o \
	lsf_add_source.o lsf_close.o lsf_delete_def.o lsf_display.o \
	lsf_disspac.o lsf_enlrec.o lsf_enq_ae_vis.o lsf_enq_cal.o \
	lsf_enq_cf.o lsf_enq_desig.o lsf_enq_int_chop.o lsf_enq_integ.o \
	lsf_enq_ion.o lsf_enq_lsf.o lsf_enq_max_rad.o lsf_enq_numbuff.o \
	lsf_enq_numsp.o lsf_enq_pc_epoch.o lsf_enq_pc_rdate.o \
	lsf_enq_samples.o lsf_enq_saved.o lsf_enq_sf.o \
	lsf_enq_smooth.o lsf_enq_spacno.o lsf_get_calgain.o \
	lsf_get_model.o lsf_get_radii.o lsf_get_range.o \
	lsf_get_sid.o lsf_get_uv.o lsf_get_vis.o lsf_init_model.o \
	lsf_list_def.o lsf_open.o lsf_prn_int_chop.o lsf_read_def.o \
	lsf_rot_vis.o lsf_save.o lsf_sel_average.o lsf_sel_cal.o \
	lsf_sel_flag.o lsf_sel_int_chop.o lsf_sel_ion_corr.o \
	lsf_sel_notspac.o lsf_sel_ph_cent.o lsf_sel_removes.o \
	lsf_sel_samples.o lsf_add_samples.o \
	lsf_sel_smooth.o lsf_sel_spacings.o \
	lsf_set_bandpass.o lsf_set_buffer.o lsf_set_int_chop.o \
	lsf_set_integt.o lsf_set_model.o lsf_set_noise.o \
	lsf_set_pbeam.o lsf_set_pc.o lsf_set_slist.o \
	lsf_set_spacings.o lsf_set_srange.o lsf_stlrec.o \
	lsf_title.o lsf_wrerr.o lsf_write_def.o \
	model_vis.o noise.o open_remcal.o pbcorr.o phase_rot.o \
	remove_source.o sample_num.o smooth.o

# Dependencies for building the object library

lsflib.a : $(OBJECT_FILES)
	ar ru lsflib.a $(OBJECT_FILES)

# Target for building the library

build : lsflib.a $(INCLUDE_FILES)

# Target for installing the library

install :
	mv lsflib.a ../liblsf.a

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > lsflib.for

# Target for splitting library into modules

split :
	fsplit lsflib.for

# Include file dependencies:

