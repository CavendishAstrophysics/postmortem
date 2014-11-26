C
C     Commands for POSTMORTEM system
C
C last change  5 Aug 2003  GP

          integer         num_cmds
          parameter     ( num_cmds = 10)

          character*70    cmd_list( num_cmds )


          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 1 )
          data            cmd_list(set_sample_file_cmd)
     * / 'set-sample-file ..... sets the current sample file'/

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 2 )
          data            cmd_list(set_lsf_cmd)
     * / 'set-lsf ............. sets the current logical sample file'/

          integer         print_obs_cmd
          parameter     ( print_obs_cmd = 3 )
          data            cmd_list(print_obs_cmd)
     * / 'print-observation ... prints the observation parameters' /

          integer         lsf_system_cmd
          parameter     ( lsf_system_cmd = 4 )
          data            cmd_list(lsf_system_cmd)
     * / 'lsf-system .......... enters the logical sample file system' /

          integer         monitor_system_cmd
          parameter     ( monitor_system_cmd = 5 )
          data            cmd_list(monitor_system_cmd)
     * / 'monitor-system ...... enters the telescope monitor system' /

          integer         mapping_system_cmd
          parameter     ( mapping_system_cmd = 6 )
          data            cmd_list(mapping_system_cmd)
     * / 'mapping-system ...... enters the mapping system' /

          integer         remove_system_cmd
          parameter     ( remove_system_cmd = 7 )
          data            cmd_list(remove_system_cmd)
     * / 'remove-system ....... enters the remove system' /

          integer         calib_system_cmd
          parameter     ( calib_system_cmd = 8 )
          data            cmd_list(calib_system_cmd)
     * / 'calibration-system .. enters the calibration system' /

          integer         ion_system_cmd
          parameter     ( ion_system_cmd = 9 )
          data            cmd_list(ion_system_cmd)
     * / 'ion-system .......... enters the ionospheric corrn. system' /
          integer         next_sample_file_cmd
          parameter      (next_sample_file_cmd = 10)
          data            cmd_list(next_sample_file_cmd) 
     * / 'next-sample-file .... set next sample file in sequence' /



