C
C     Commands for Monitor section of the POSTMORTEM system
C
          integer         num_cmds
          parameter     ( num_cmds = 9 )

          character*70    cmd_list( num_cmds )

          integer         exit_cmd
          parameter     ( exit_cmd = 0 )

          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 1 )
          data            cmd_list(set_sample_file_cmd)
     * / 'set-sample-file ..... sets the current default sample file' /

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 2 )
          data            cmd_list(set_lsf_cmd)
     * / 'set-lsf ............. sets the current default LSF' /

          integer         dump_spacings_cmd
          parameter     ( dump_spacings_cmd = 3 )
          data            cmd_list(dump_spacings_cmd)
     * / 'dump-spacings ....... prints raw spacings information' /

          integer         check_agcs_cmd
          parameter     ( check_agcs_cmd = 4 )
          data            cmd_list(check_agcs_cmd)
     * / 'check-agcs .......... reports AGC readings during the run' /

          integer         check_pcs_cmd
          parameter     ( check_pcs_cmd = 5 )
          data            cmd_list(check_pcs_cmd)
     * / 'check-pcs ........... reports PC readings during the run' /

          integer         check_pointing_cmd
          parameter     ( check_pointing_cmd = 6 )
          data            cmd_list(check_pointing_cmd)
     * / 'check-pointing ...... reports aerial pointing' /

          integer         print_geometry_cmd
          parameter     ( print_geometry_cmd =  7)
          data            cmd_list(print_geometry_cmd)
     * / 'print-geometry ...... print the telescope geometry '/

          integer         print_config_cmd
          parameter     ( print_config_cmd =  8)
          data            cmd_list(print_config_cmd)
     * / 'print-configuration . print the telescope configuration'/

          integer         save_sample_file_cmd
          parameter     ( save_sample_file_cmd = 9 )
          data            cmd_list(save_sample_file_cmd)
     * / 'save-sample-file .... disk to disk copy of a sample file' /

C         integer         #_cmd
C         parameter     ( #_cmd =  )
C         data            cmd_list(#_cmd)
C    * / '# ......' /

