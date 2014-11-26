# Makefile for the MRAO POSTMORTEM SAMPLIB library
# 3 March 99

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\

OBJECT_FILES=\
	close_sf.o close_source.o create_ion_corr.o create_source.o \
	enq_ae_group.o enq_ae_hut.o enq_ae_spac.o enq_ae_vis.o \
	enq_aestat.o enq_baselines.o enq_centres.o enq_chfreq.o \
	enq_chgeom.o enq_clip.o enq_ctab_pack.o enq_epoch.o enq_freq.o \
	enq_geometry.o enq_groups.o enq_grp_desc.o \
	enq_gt_rec.o enq_hut_ae.o enq_iae_code.o enq_iba_code.o \
	enq_iba_freq.o enq_ich_code.o enq_ich_freq.o enq_integ.o \
	enq_namfil.o enq_ion_corr.o enq_ion_name.o enq_ion_num.o \
	enq_isp_code.o enq_loc_time.o enq_max_point.o \
	enq_mjd_st0.o enq_mjdstart.o enq_numcorr.o \
	enq_numsamp.o enq_numsp.o enq_numsrc.o enq_numvis.o \
	enq_obsdef.o enq_off_tables.o enq_offsets.o enq_pages.o \
	enq_path_comp.o enq_pc_epoch.o enq_phys_tscope.o enq_point.o \
	enq_poln.o enq_ratel.o enq_save_flag.o enq_sid_time.o \
	enq_sfdir.o enq_sfname.o enq_sftype.o \
	enq_spac_no.o enq_src_def.o enq_src_integ.o \
	enq_src_name.o enq_src_pack.o \
	enq_teldef.o enq_telgeom.o enq_tscope.o enq_tsys.o \
	enq_units.o enq_v1_ae_spac.o enq_v1_ae_vis.o enq_v1_aestat.o \
	enq_v1_base.o enq_v1_centre.o enq_v1_chgeom.o enq_v1_clip.o \
	enq_v1_epoch.o enq_v1_freq.o enq_v1_geometry.o enq_v1_iae_code.o \
	enq_v1_iba_code.o enq_v1_iba_freq.o enq_v1_ich_code.o \
	enq_v1_ich_freq.o enq_v1_integ.o enq_v1_isp_code.o \
	enq_v1_loctim.o enq_v1_mjd0.o enq_v1_mxpoint.o \
	enq_v1_numsp.o enq_v1_numvis.o enq_v1_obsdef.o enq_v1_offsets.o \
	enq_v1_path_comp.o enq_v1_point.o enq_v1_poln.o enq_v1_saved.o \
	enq_v1_sidtim.o enq_v1_spac_no.o enq_v1_src_pack.o enq_v1_teldef.o \
	enq_v1_tscope.o enq_v1_tsys.o enq_v1_units.o enq_v1_zfile.o \
	enq_v2_ae_spac.o enq_v2_ae_vis.o enq_v2_aestat.o enq_v2_base.o \
	enq_v2_centre.o enq_v2_chfreq.o enq_v2_chgeom.o enq_v2_clip.o \
	enq_v2_epoch.o enq_v2_freq.o enq_v2_geometry.o enq_v2_gt_rec.o \
	enq_v2_iae_code.o enq_v2_iba_code.o enq_v2_iba_freq.o \
	enq_v2_ich_code.o enq_v2_ich_freq.o enq_v2_integ.o \
	enq_v2_isp_code.o enq_v2_loctim.o enq_v2_mjd0.o enq_v2_mxpoint.o \
	enq_v2_numsp.o enq_v2_numvis.o enq_v2_obsdef.o enq_v2_offsets.o \
	enq_v2_off_tabs.o enq_v2_path_comp.o enq_v2_pc.o enq_v2_point.o \
	enq_v2_poln.o enq_v2_ratel.o enq_v2_saved.o enq_v2_sidtim.o \
	enq_v2_spac_no.o enq_v2_src_pack.o enq_v2_teldef.o enq_v2_telgeom.o \
	enq_v2_tscope.o enq_v2_tsys.o enq_v2_units.o enq_vis_desig.o \
	enq_2_epochs.o enq_v1_2_epochs.o enq_v2_2_epochs.o \
	enq_xyz.o \
	ex_samp.o exs_delete.o exs_disc.o exs_print.o \
	exs_print_clfst.o exs_print_ryle.o exs_read.o \
	find_sources.o get_aerials.o get_huts.o get_merge.o get_sf_pack.o \
	get_source.o get_spacings.o mean_spac.o merge_vis_buffer.o \
	next_sample_file.o \
        open_sf.o open_source.o open_srckey.o put_sf_pack.o read_buffer.o \
	read_ct.o read_ion_corr.o read_ion_ct.o read_monitor.o \
	read_rt.o read_rt_sid.o read_sample.o read_sample_sid.o \
	read_zero_corr.o rfile_log.o scan_source_list.o \
	separation.o set_aerials.o set_ctab_pack.o set_huts.o \
	set_ion_corr.o set_merge.o set_monitor.o set_numsamp.o \
	set_save_flag.o set_spacings.o set_src_def.o set_src_pack.o \
	set_v1_merge.o set_v1_monitor.o set_v1_nsamp.o set_v1_saved.o \
	set_v2_merge.o set_v2_monitor.o set_v2_nsamp.o set_v2_saved.o \
	smp_wrerr.o wfile_log.o write_buffer.o write_ct.o write_ion_corr.o \
	write_ion_ct.o write_rt.o write_sample.o yesno_num.o rfile_log.o

# Dependencies for building the object library

samplib.a : $(OBJECT_FILES)
	ar ru samplib.a $(OBJECT_FILES)

# Target for building the library

build : samplib.a $(INCLUDE_FILES)

# Target for installing the library

install : samplib.a
	mv samplib.a ../libsamp.a

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > samplib.for

# Target for splitting library into modules

split :
	fsplit samplib.for

# Include file dependencies:

