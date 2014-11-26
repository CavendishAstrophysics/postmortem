C
C Basic commands of the POSTMORTEM system
C ---------------------------------------
C
          integer         num_basic_cmds
          parameter     ( num_basic_cmds = 23)

          character*70    basic_cmd_list( num_basic_cmds )


          integer         banner_cmd
          parameter     ( banner_cmd = 1 )
          data            basic_cmd_list(banner_cmd)
     * / 'display-banner ...... display postmortem banner'/

          integer         version_cmd
          parameter     ( version_cmd = 2 )
          data            basic_cmd_list(version_cmd)
     * / 'display-version ..... display version number and date'/

          integer         examine_lsf_cmd
          parameter     ( examine_lsf_cmd = 3 )
          data            basic_cmd_list(examine_lsf_cmd)
     * / 'examine-lsf ......... executes the examine-lsf command' /

          integer         examine_maps_cmd
          parameter     ( examine_maps_cmd = 4 )
          data            basic_cmd_list(examine_maps_cmd)
     * / 'examine-maps ........ executes the examine-maps command' /

          integer         examine_sample_cmd
          parameter     ( examine_sample_cmd = 5 )
          data            basic_cmd_list(examine_sample_cmd)
     * / 'examine-sample ...... executes the examine-sample command' /

          integer         list_basic_cmd
          parameter     ( list_basic_cmd = 6 )
          data            basic_cmd_list(list_basic_cmd)
     * / 'list-basic-commands . list the basic commands ' /

          integer         find_source_cmd
          parameter     ( find_source_cmd = 7 )
          data            basic_cmd_list(find_source_cmd)
     * /'scan-source-list .... scans source lists for selected sources'/

          integer         set_dir_cmd
          parameter     ( set_dir_cmd = 8 )
          data            basic_cmd_list(set_dir_cmd)
     * / 'set-directory ....... set sample file directory'/

          integer         disopts_cmd
          parameter     ( disopts_cmd = 9 )
          data            basic_cmd_list(disopts_cmd)
     * / 'set-display-options . set options for the display-commands'/

          integer         set_plot_cmd
          parameter     ( set_plot_cmd = 10 )
          data            basic_cmd_list(set_plot_cmd)
     * / 'set-plot-device ..... sets the current plot device' /

          integer         sidereal_time_cmd
          parameter     ( sidereal_time_cmd = 11 )
          data            basic_cmd_list(sidereal_time_cmd)
     * / 'sidereal-time ....... prints the current sidereal time' /

          integer         batchsys_cmd
          parameter     ( batchsys_cmd = 12 )
          data            basic_cmd_list(batchsys_cmd)
     * / 'batch-monitor ....... enter batch system monitor sub-system'/

          integer         filesys_cmd
          parameter     ( filesys_cmd = 13 )
          data            basic_cmd_list(filesys_cmd)
     * / 'file-system ......... enter filing system sub-system'/

          integer         cal_sys_cmd
          parameter     ( cal_sys_cmd = 14 )
          data            basic_cmd_list(cal_sys_cmd)
     * / 'calibration-system .. enters the calibration system' /

          integer         ion_sys_cmd
          parameter     ( ion_sys_cmd = 15 )
          data            basic_cmd_list(ion_sys_cmd)
     * / 'ion-system .......... enters the ionospheric corrn. system' /

          integer         lsf_sys_cmd
          parameter     ( lsf_sys_cmd = 16 )
          data            basic_cmd_list(lsf_sys_cmd)
     * / 'lsf-system .......... enters the logical sample file system' /

          integer         map_sys_cmd
          parameter     ( map_sys_cmd = 17 )
          data            basic_cmd_list(map_sys_cmd)
     * / 'mapping-system ...... enters the mapping system' /

          integer         mon_sys_cmd
          parameter     ( mon_sys_cmd = 18 )
          data            basic_cmd_list(mon_sys_cmd)
     * / 'monitor-system ...... enters the telescope monitor system' /

          integer         rem_sys_cmd
          parameter     ( rem_sys_cmd = 19 )
          data            basic_cmd_list(rem_sys_cmd)
     * / 'remove-system ....... enters the remove system' /

          integer         help_cmd
          parameter     ( help_cmd = 20)
          data            basic_cmd_list(help_cmd)
     * / 'help ................ extensive help on postmortem commands'/

          integer         news_cmd
          parameter     ( news_cmd = 21 )
          data            basic_cmd_list(news_cmd)
     * / 'news ................ prints the current system news'/

          integer         metafile_cmd
          parameter     ( metafile_cmd = 22 )
          data            basic_cmd_list(metafile_cmd)
     * / 'view-metafile ....... view a metafile on a graphics device'/

          integer         exit_cmd
          parameter     ( exit_cmd = 23 )
          data            basic_cmd_list(exit_cmd)
     * / 'exit ................ exits from the current system'/

