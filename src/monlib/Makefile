# Makefile for the MRAO POSTMORTEM MONLIB library
#

FFLAGC=-u
FCOMPL=f77

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<

INCLUDE_FILES=\

OBJECT_FILES=\
	anal_holog_ryle.o anal_point_ryle.o apply_patch_ryle.o \
	check_aes_clfst.o check_agcs_clfst.o check_alcs_ryle.o \
	check_corr_clfst.o check_cryo_ryle.o check_huts_clfst.o \
	check_inter_clfst.o \
	check_misc_ryle.o check_pcs_clfst.o check_point_clfst.o \
	check_phe_ryle.o check_point_ryle.o check_rain_gauge.o \
	check_vac_ryle.o check_wind_gauge.o copy_samp_ryle.o \
	dump_spacings.o enq_mon_agc.o enq_mon_cryo.o \
	enq_mon_hadec.o enq_mon_misc.o enq_mon_pc.o \
	enq_mon_phe.o enq_mon_rain.o enq_mon_rfac.o enq_mon_vac.o \
	enq_mon_wind.o enq_mon_update.o enq_samp_rt.o \
	enq_v1_mon_agc.o enq_v1_mon_hadec.o enq_v1_mon_pc.o \
	enq_v1_mon_update.o enq_v2_mon_agc.o enq_v2_mon_cryo.o \
	enq_v2_mon_hadec.o enq_v2_mon_misc.o enq_v2_mon_phe.o \
	enq_v2_mon_rain.o enq_v2_mon_rfac.o enq_v2_mon_vac.o \
	enq_v2_mon_wind.o fix_smooth_rate.o flag_point_ryle.o \
	list_spac_clfst.o log_comment.o mon_title.o mon_wrerr.o \
	obs_stat_clfst.o plot_agcs_clfst.o plot_alcs_ryle.o \
	plot_cryo_ryle.o plot_misc_ryle.o plot_pcs_clfst.o \
	plot_phe_ryle.o plot_point_clfst.o plot_point_ryle.o \
	plot_rain_gauge.o plot_vac_ryle.o plot_wind_gauge.o \
	plot_wts_clfst.o print_aes_clfst.o print_ampf_clfst.o \
	print_coll_clfst.o print_config_ryle.o print_ct_clfst.o \
	print_geom_clfst.o print_geom_ryle.o print_geometry.o \
	print_huts_clfst.o print_list.o print_obs_pars.o \
	print_obs_status.o print_patch_ryle.o print_sid_time.o \
	print_zeros_clfst.o recover_samp_file.o reset_samp_count.o \
	save_samp_clfst.o save_samp_ryle.o scan_aes_clfst.o \
	scan_agcs_clfst.o scan_alcs_ryle.o scan_cryo_ryle.o \
	scan_misc_ryle.o scan_pcs_clfst.o scan_phe_ryle.o \
	scan_point_clfst.o scan_point_ryle.o scan_rain_gauge.o \
	scan_vac_ryle.o scan_wind_gauge.o \
	scan_sf_clfst.o set_comment_ryle.o show_agcs_clfst.o \
	show_alcs_ryle.o show_cryo_ryle.o show_misc_ryle.o \
	show_pcs_clfst.o show_phe_ryle.o show_point_clfst.o \
	show_point_ryle.o  show_rain_gauge.o show_vac_ryle.o \
	table_aes.o table_data.o table_huts_clfst.o \
	table_log_ryle.o table_spac_clfst.o table_spac_ryle.o \
	table_vis_ryle.o write_zeros_clfst.o \
	flag_shad_ryle.o calc_shadow.o shadow_flag.o

# Dependencies for building the object library

monlib.a : $(OBJECT_FILES)
	ar ru monlib.a $(OBJECT_FILES)

# Target for building the library

build : monlib.a $(INCLUDE_FILES)

# Target for installing the library

install : monlib.a
	mv monlib.a ../libmon.a

# Target for cleaning up the source directory

clean :
	rm -f $(OBJECT_FILES)

# Target for building single-source module

combine :
	cat *.f > monlib.for

# Target for splitting library into modules

split :
	fsplit monlib.for

# Include file dependencies:

