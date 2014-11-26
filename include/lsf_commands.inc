C
C     Commands for the logical sample file section of POSTMORTEM
C

          integer         num_cmds
          parameter      (num_cmds = 39)

          character*70    cmd_list (num_cmds)

          integer         exit_cmd
          parameter      (exit_cmd = 0)

          integer         set_sample_file_cmd
          parameter      (set_sample_file_cmd = 1)
          data            cmd_list (set_sample_file_cmd)
     * / 'set-sample-file ..... sets the current sample file' /

          integer         set_lsf_cmd
          parameter      (set_lsf_cmd = 2)
          data            cmd_list (set_lsf_cmd)
     * / 'open-lsf ............ opens an existing logical sample file' /

          integer         lsf_save_cmd
          parameter      (lsf_save_cmd = 3)
          data            cmd_list(lsf_save_cmd)
     * / 'save-lsf ............ saves the current logical sample file' /

          integer          print_lsf_cmd
          parameter       (print_lsf_cmd = 4)
          data             cmd_list(print_lsf_cmd)
     * / 'print-lsf ........... prints a description of the lsf' /

          integer          list_lsf_cmd
          parameter       (list_lsf_cmd = 5)
          data             cmd_list(list_lsf_cmd)
     * / 'list-lsfs ........... lists descriptions of the lsfs' /

          integer          delete_lsf_cmd
          parameter       (delete_lsf_cmd = 6)
          data             cmd_list(delete_lsf_cmd)
     * / 'delete-lsf .......... mark saved LSF as deleted' /

          integer          undelete_lsf_cmd
          parameter       (undelete_lsf_cmd = 7)
          data             cmd_list(undelete_lsf_cmd)
     * / 'undelete-lsf ........ mark saved LSF as not deleted' /

          integer         select_ion_corr_cmd
          parameter      (select_ion_corr_cmd = 8)
          data            cmd_list(select_ion_corr_cmd)
     * / 'select-ion-correction sets the ionospheric correction' /

          integer         select_calib_cmd
          parameter      (select_calib_cmd = 9)
          data            cmd_list(select_calib_cmd)
     * / 'select-calibration .. sets the calibration to use in the lsf'/

          integer         select_removes_cmd
          parameter      (select_removes_cmd = 10)
          data            cmd_list(select_removes_cmd)
     * / 'select-removes ...... sets the remove sources to use' /

          integer         select_flagging_cmd
          parameter      (select_flagging_cmd = 11)
          data            cmd_list(select_flagging_cmd)
     * / 'select-flagging ..... sets the flagging version to use'/

          integer         select_int_chop_cmd
          parameter      (select_int_chop_cmd = 12)
          data            cmd_list(select_int_chop_cmd)
     * / 'interference-chop ... sets the lsf interference chop levels' /

          integer         select_smooth_cmd
          parameter      (select_smooth_cmd = 13)
          data            cmd_list(select_smooth_cmd)
     * / 'smoothing ........... sets the lsf smoothing and sampling' /

          integer         select_average_cmd
          parameter      (select_average_cmd = 14)
          data            cmd_list(select_average_cmd)
     * / 'averaging ........... sets the averaging type for smoothing' /

          integer         select_ph_cent_cmd
          parameter      (select_ph_cent_cmd = 15)
          data            cmd_list(select_ph_cent_cmd)
     * / 'shift-phase-centre .. sets the lsf phase centre' /

          integer         select_samples_cmd
          parameter      (select_samples_cmd = 16)
          data            cmd_list (select_samples_cmd)
     * / 'sampling ............ sets the time sampling of the lsf' /

          integer         select_spacings_cmd
          parameter      (select_spacings_cmd = 17)
          data            cmd_list (select_spacings_cmd)
     * / 'spacing-list ........ sets the spacing list of the lsf' /

          integer         select_notspac_cmd
          parameter      (select_notspac_cmd = 18)
          data            cmd_list (select_notspac_cmd)
     * / 'not-spacings ........ excludes spacings from the lsf' /

          integer         display_noise_cmd
          parameter      (display_noise_cmd = 19)
          data            cmd_list(display_noise_cmd)
     * / 'display-noise ....... displays noise by subtracting samples' /

          integer         display_hist_cmd
          parameter      (display_hist_cmd = 20)
          data            cmd_list(display_hist_cmd)
     * / 'display-histogram ... displays histogram of data' /

          integer         display_sample_cmd
          parameter      (display_sample_cmd = 21)
          data            cmd_list(display_sample_cmd)
     * / 'display-sample ...... displays spacings for a given sample' /

          integer         display_spacing_cmd
          parameter      (display_spacing_cmd = 22)
          data            cmd_list(display_spacing_cmd)
     * / 'display-spacing ..... displays vis. data at a given spacing' /

          integer         display_subband_cmd
          parameter      (display_subband_cmd = 23)
          data            cmd_list(display_subband_cmd)
     * / 'display-channel ..... displays vis. data for channels' /

          integer         display_grey_cmd
          parameter      (display_grey_cmd = 24)
          data            cmd_list(display_grey_cmd)
     * / 'display-TV .......... TV display of sample-file (512x512)' /

          integer         display_fft_cmd
          parameter      (display_fft_cmd = 25)
          data            cmd_list(display_fft_cmd)
     * / 'display-fft ......... displays the fft of a given sample' /

          integer         display_cont_fft_cmd
          parameter      (display_cont_fft_cmd = 26)
          data            cmd_list(display_cont_fft_cmd)
     * / 'display-cont-fft .... simultaneous display of sample ffts' /

          integer          fit_spac_cmd
          parameter       (fit_spac_cmd = 27)
          data             cmd_list(fit_spac_cmd)
     * / 'fit-spacings  ....... fit phase of spacing data to model'/

          integer          grey_merge_cmd
          parameter       (grey_merge_cmd = 28)
          data             cmd_list(grey_merge_cmd)
     */ 'grey-merge ........... grey-scale display of merged spacings' /

          integer         merge_spacings_cmd
          parameter      (merge_spacings_cmd = 29)
          data            cmd_list(merge_spacings_cmd)
     * / 'merge-spacings ...... plots merged spacing data vs. time' /

          integer         model_spacings_cmd
          parameter      (model_spacings_cmd = 30)
          data            cmd_list(model_spacings_cmd)
     */'model-spacings ...... plots merged model spacing data vs. time'/

          integer          print_spacings_cmd
          parameter       (print_spacings_cmd = 31)
          data             cmd_list(print_spacings_cmd)
     * / 'print-spacings ...... prints merged spacing data vs. time'/

          integer          scan_inter_cmd
          parameter       (scan_inter_cmd = 32)
          data             cmd_list(scan_inter_cmd)
     * / 'scan-interference ... totals number of rejected visibilities'/

          integer          scan_sample_cmd
          parameter       (scan_sample_cmd = 33)
          data             cmd_list(scan_sample_cmd)
     * / 'scan-sample-file .... prints merged visibility data' /

          integer          list_sp_cmd
          parameter       (list_sp_cmd = 34)
          data             cmd_list(list_sp_cmd)
     * / 'list-spacings ....... list spacings for an RT sample file'/

          integer          print_obs_cmd
          parameter       (print_obs_cmd = 35)
          data             cmd_list(print_obs_cmd)
     * / 'print-observation ... prints the observation parameters' /

          integer          flag_sys_cmd
          parameter       (flag_sys_cmd = 36)
          data             cmd_list(flag_sys_cmd)
     * / 'flag-system ......... enter flag sub-system'/

          integer         add_samples_cmd
          parameter      (add_samples_cmd = 37)
          data            cmd_list (add_samples_cmd)
     * / 'add-samples ......... includes more samples in the lsf' /

          integer         archive_flux_cmd
          parameter      (archive_flux_cmd = 38)
          data            cmd_list(archive_flux_cmd)
     * / 'archive-flux ........ write record of flux density' /

          integer         phase_fit_cmd
          parameter      (phase_fit_cmd = 39)
          data            cmd_list(phase_fit_cmd)
     * / 'phase-fit ........... switch on/off fitting to phase' /

