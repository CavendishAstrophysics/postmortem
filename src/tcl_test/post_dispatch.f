       subroutine post_dispatch( interp, cs, lcs, s )
C      ----------------------------------------------
C
C Command dispatcher for Postmortem.
       integer         interp(*)
       character*1024  cs
       integer         lcs, s
C
C     Cambridge Synthesis Telescopes Observation Reduction Package
C
C     Version X0.1
C

C Local constant and variable declarations
C
C     Constants
          include '/mrao/post/include/post_common.inc'
          include '/mrao/post/include/post_sys_pars.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/include/chrlib_functions.inc'
          include 'post_commands.inc'

C Sample file names.
          character*(80)  def_sf, new_sf
C LSF numbers and keys.
          integer         def_lsf, new_lsf, lsf_num, sf_lun
C Current command.
          integer         command
C results string
          integer         iocmd_results(258)
C local variables used by show-sidereal-time command
          character*80    string
          integer         ls, sid_time(3)
C save control data in common
          common /tclpost/ def_sf, def_lsf

       
       call anm_disstart( interp, cs, lcs, s )
       call io_getcmd('Post-top> ', cmd_list, num_cmds, command, s)

       if (s .ne. 0) then
         continue

       else if ( command .eq. set_sample_file_cmd ) then
         new_sf  = ' '
         new_lsf = -1
         call open_sf ( sf_lun, new_sf, 'READ', 0, s )
         if ( s .eq. 0 ) then
            call close_sf( sf_lun, s )
            def_sf = new_sf
            def_lsf = -1
            call io_wrout( ' ')
            call io_wrout( def_sf )
            call io_wrout( ' ')
            call lsf_open( def_sf, def_lsf, 'READ', lsf_num, s )
            call lsf_close( lsf_num, s )
         end if

       else if (command .eq. set_lsf_cmd) then
         new_lsf = 0
         call lsf_open( def_sf, new_lsf, 'READ', lsf_num, s )
         call lsf_close( lsf_num, s )
         if ( s .eq. 0 ) then
            def_lsf = new_lsf
         elseif ( s .eq. NO_LSFSAVED ) then
            call io_wrout( 'No logical sample files are saved.' )
            s = 0
         end if

       else if ( command .eq. print_obs_cmd ) then
         call print_obs_pars( interp, iocmd_results, def_sf, s )

       else if ( command .eq. lsf_system_cmd ) then
         call lsf_system( interp, iocmd_results, def_sf, def_lsf, s )

       else if ( command .eq. monitor_system_cmd ) then
         call monitor_system( interp, iocmd_results, def_sf, def_lsf, s)

       else if ( command .eq. mapping_system_cmd ) then
         call mapping_system( interp, iocmd_results, def_sf, def_lsf, s)

       else if ( command .eq. remove_system_cmd ) then
         call remove_system( interp, iocmd_results, def_sf, def_lsf, s )

       else if ( command .eq. calib_system_cmd ) then
          call calib_system( interp, iocmd_results, def_sf, def_lsf, s )

       else if ( command .eq. ion_system_cmd ) then
          call ion_system( interp, iocmd_results, def_sf, def_lsf, s )
       
       else if ( command .eq. set_dir_cmd ) then
          call io_getfil('Sample file directory : ', '*', def_dir, s)

       else if ( command .eq. set_plot_cmd ) then
          call io_getplt('Plot-device : ', '*', plot_device, s)

       else if ( command .eq. sidereal_time_cmd ) then
          call util_enqsid(sid_time)
          call chr_chtime(sid_time, string, ls)
          call io_wrout('Sidereal time now is  '//string(1:ls)//' ST')
          call io_wrout( ' ' )

      else if ( command .eq. disopts_cmd ) then
          call plot_getopts( plot_device, s )

       end if
       call anm_disend( interp, iocmd_results, cs, lcs, s )
       end




