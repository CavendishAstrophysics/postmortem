C
C     Commands for Monitor section of the POSTMORTEM system
C
C     Control tables version 1 (CLFST)
C
          integer         num_cmds
          parameter     ( num_cmds = 23 )

          character*70    cmd_list( num_cmds )

          integer         exit_cmd
          parameter     ( exit_cmd = 0 )

          integer         dump_spacings_cmd
          parameter     ( dump_spacings_cmd = 1 )
          data            cmd_list(dump_spacings_cmd)
     * / 'dump-spacings ....... prints raw spacings information' /

          integer         list_spacings_cmd
          parameter     ( list_spacings_cmd = 2 )
          data            cmd_list(list_spacings_cmd)
     * / 'list-spacings ....... lists telescope spacings for the run' /

          integer         print_aerial_status_cmd
          parameter     ( print_aerial_status_cmd = 3 )
          data            cmd_list(print_aerial_status_cmd)
     * / 'print-aerial-status . prints the aerial status for the run' /

          integer         print_ampf_cmd
          parameter     ( print_ampf_cmd = 4 )
          data            cmd_list(print_ampf_cmd)
     * / 'print-amplitude-factors . prints aerial amplitude factors' /

          integer         print_coll_cmd
          parameter     ( print_coll_cmd = 5 )
          data            cmd_list(print_coll_cmd)
     * / 'print-phase-corrections . prints aerial collimation phases' /

          integer         print_control_tables_cmd
          parameter     ( print_control_tables_cmd = 6 )
          data            cmd_list(print_control_tables_cmd)
     * / 'print-control-tables  prints the run''s control tables' /

          integer         print_hut_status_cmd
          parameter     ( print_hut_status_cmd = 7 )
          data            cmd_list(print_hut_status_cmd)
     * / 'print-hut-status .... prints the hut status during the run' /

          integer         print_geometry_cmd
          parameter     ( print_geometry_cmd = 8 )
          data            cmd_list(print_geometry_cmd)
     * / 'print-geometry ...... prints the telescope geometry' /

          integer         print_obs_cmd
          parameter     ( print_obs_cmd = 9 )
          data            cmd_list(print_obs_cmd)
     * / 'print-observation ... prints the observation parameters' /

          integer         print_sidereal_time_cmd
          parameter     ( print_sidereal_time_cmd = 10 )
          data            cmd_list(print_sidereal_time_cmd)
     * / 'print-sidereal-time . prints sidereal time for samples' /

          integer         print_zero_corr_cmd
          parameter     ( print_zero_corr_cmd = 11 )
          data            cmd_list(print_zero_corr_cmd)
     * / 'print-zero-corr ..... prints zero corrections for the run' /

          integer         check_agcs_cmd
          parameter     ( check_agcs_cmd = 12 )
          data            cmd_list(check_agcs_cmd)
     * / 'check-agcs .......... reports AGC readings during the run' /

          integer         check_corr_cmd
          parameter     ( check_corr_cmd = 13 )
          data            cmd_list(check_corr_cmd)
     * / 'check-correlator .... reports correlator errors' /

          integer         check_inter_cmd
          parameter     ( check_inter_cmd = 14 )
          data            cmd_list(check_inter_cmd)
     * / 'check-interference .. reports interference checking' /

          integer         check_pcs_cmd
          parameter     ( check_pcs_cmd = 15 )
          data            cmd_list(check_pcs_cmd)
     * / 'check-pcs ........... reports PC readings during the run' /

          integer         check_pointing_cmd
          parameter     ( check_pointing_cmd = 16 )
          data            cmd_list(check_pointing_cmd)
     * / 'check-pointing ...... reports aerial pointing' /

          integer         recover_samp_cmd
          parameter     ( recover_samp_cmd = 17 )
          data            cmd_list(recover_samp_cmd)
     * / 'recover-sample-file . recovers sample count from aborted run'/

          integer         reset_samp_count_cmd
          parameter     ( reset_samp_count_cmd = 18 )
          data            cmd_list(reset_samp_count_cmd)
     * / 'reset-sample-count .. resets the maximum sample of the run' /

          integer         scan_aerials_cmd
          parameter     ( scan_aerials_cmd = 19 )
          data            cmd_list(scan_aerials_cmd)
     * / 'scan-aerials ........ averages visibilities by aerial' /

          integer         scan_sample_file_cmd
          parameter     ( scan_sample_file_cmd = 20 )
          data            cmd_list(scan_sample_file_cmd)
     * / 'scan-sample-file .... averages parameters over the run' /

          integer         save_sample_file_cmd
          parameter     ( save_sample_file_cmd = 21 )
          data            cmd_list(save_sample_file_cmd)
     * / 'save-sample-file .... disk to disk copy of a sample file' /

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 22 )
          data            cmd_list(set_lsf_cmd)
     * / 'set-lsf ............. sets the current default LSF' /

          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 23 )
          data            cmd_list(set_sample_file_cmd)
     * / 'set-sample-file ..... sets the current default sample file' /

