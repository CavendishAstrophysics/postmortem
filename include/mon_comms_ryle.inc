C
C     Commands for Monitor section of the POSTMORTEM system
C
C     Control tables version 2 (RYLE telescope)

          integer         num_cmds
          parameter      (num_cmds = 27) 

          character*70    cmd_list (num_cmds) 

          integer         exit_cmd
          parameter      (exit_cmd = 0) 

          integer         anal_holog_cmd
          parameter      (anal_holog_cmd = 1) 
          data            cmd_list(anal_holog_cmd)
     * / 'analyse-holography .. analyses holography test observation'/

          integer         anal_point_cmd
          parameter      (anal_point_cmd = 2) 
          data            cmd_list(anal_point_cmd)
     * / 'analyse-pointing .... analyses pointing test observation'/

          integer         apply_patch_cmd
          parameter      (apply_patch_cmd = 3) 
          data            cmd_list(apply_patch_cmd)
     * / 'apply-patch ......... applies patch to control tables'/

          integer         check_cryo_cmd                             
          parameter      (check_cryo_cmd = 4) 
          data            cmd_list(check_cryo_cmd)
     * / 'check-cryo-temp ..... reports cryo temperature readings' /
   
          integer         check_IFalc_cmd
          parameter      (check_IFalc_cmd = 5) 
          data            cmd_list(check_IFalc_cmd)
     * / 'check-IFalcs ........ reports IF ALC readings' /  

          integer         check_LOalc_cmd
          parameter      (check_LOalc_cmd = 6) 
          data            cmd_list(check_LOalc_cmd)
     * / 'check-LOalcs ........ reports LO ALC readings' /

          integer         check_phe_cmd
          parameter      (check_phe_cmd = 7) 
          data            cmd_list(check_phe_cmd)
     * / 'check-helium ........ reports Helium pressure readings' /

          integer         check_misc_cmd
          parameter      (check_misc_cmd = 8) 
          data            cmd_list(check_misc_cmd)
     * / 'check-parameters .... reports miscellaneous parameters' /

          integer         check_point_cmd
          parameter      (check_point_cmd = 9) 
          data            cmd_list(check_point_cmd)
     * / 'check-pointing ...... reports aerial pointing errors' /

          integer         check_rain_cmd
          parameter      (check_rain_cmd = 10) 
          data            cmd_list(check_rain_cmd)
     * / 'check-rain-gauge .... reports rain gauge readings' /

          integer         check_vac_cmd
          parameter      (check_vac_cmd = 11) 
          data            cmd_list(check_vac_cmd)
     * / 'check-vacuum ........ reports vacuum readings' /

          integer         check_wind_cmd
          parameter      (check_wind_cmd = 12) 
          data            cmd_list(check_wind_cmd)
     * / 'check-wind-gauge .... reports wind gauge readings' /

          integer         copy_sample_file_cmd
          parameter      (copy_sample_file_cmd = 13) 
          data            cmd_list(copy_sample_file_cmd)
     * / 'copy-sample-file .... disk to disk copy of a sample file' /

          integer         dump_spacings_cmd
          parameter      (dump_spacings_cmd = 14) 
          data            cmd_list(dump_spacings_cmd)
     * / 'dump-spacings ....... prints raw spacings information' /

          integer         flag_point_cmd
          parameter      (flag_point_cmd = 15) 
          data            cmd_list(flag_point_cmd)
     * / 'flag-pointing ....... flags aerial pointing errors' /

          integer         log_comment_cmd
          parameter      (log_comment_cmd = 16) 
          data            cmd_list(log_comment_cmd)
     * / 'log-comment ......... adds a comment to the observation log' /

          integer         print_config_cmd
          parameter      (print_config_cmd = 17)
          data            cmd_list(print_config_cmd)
     * / 'print-configuration . prints the telescope configuration'/

          integer         print_geometry_cmd
          parameter      (print_geometry_cmd = 18)
          data            cmd_list(print_geometry_cmd)
     * / 'print-geometry ...... prints the telescope geometry'/

          integer         print_obs_cmd
          parameter      (print_obs_cmd = 19)
          data            cmd_list(print_obs_cmd)
     * / 'print-observation ... prints the observation parameters'/

          integer         print_patch_cmd
          parameter      (print_patch_cmd = 20)
          data            cmd_list(print_patch_cmd)
     * / 'print-patch-level ... prints patches applied to sample file'/

          integer         recover_samp_cmd
          parameter      (recover_samp_cmd = 21) 
          data            cmd_list(recover_samp_cmd)
     * / 'recover-sample-file . recovers sample count from aborted run'/

          integer         reset_samp_count_cmd
          parameter      (reset_samp_count_cmd = 22) 
          data            cmd_list(reset_samp_count_cmd)
     * / 'reset-sample-count .. resets the maximum sample of the run' /

          integer         save_sample_file_cmd
          parameter      (save_sample_file_cmd = 23) 
          data            cmd_list(save_sample_file_cmd)
     * /'save-sample-file .... copy observation to target sample files'/

          integer         set_comment_cmd
          parameter      (set_comment_cmd = 24) 
          data            cmd_list(set_comment_cmd)
     * / 'set-comment ......... adds comment to the sample file header'/

          integer         set_lsf_cmd
          parameter      (set_lsf_cmd = 25) 
          data            cmd_list(set_lsf_cmd)
     * / 'set-lsf ............. sets the current default LSF' /

          integer         set_sample_file_cmd
          parameter      (set_sample_file_cmd = 26) 
          data            cmd_list(set_sample_file_cmd)
     * / 'set-sample-file ..... sets the current default sample file' /

          integer         shadow_cmd
          parameter      (shadow_cmd = 27) 
          data            cmd_list(shadow_cmd)
     * / 'shadow .............. work out shadowing times' /

