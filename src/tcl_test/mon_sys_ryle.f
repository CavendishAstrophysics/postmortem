C
C+mon_sys_ryle
C
      SUBROUTINE mon_sys_ryle( def_sf, def_lsf, command, s )

C
C     The monitor system within postmortem.  RYLE telescope version.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current logical sample file key
              integer             def_lsf

C     Returned:
C         Current command
              integer             command
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the RYLE monitor system commands in one place.
C
C-
C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/mon_comms_ryle.inc'
          include '/mrao/post/include/mon_sys_pars.inc'
          include '/mrao/post/include/post_common.inc'

C     Variables, equivalences and commons
C         New sample file name and lsf key
              character*64    new_sf
              integer         new_lsf
C         Logical sample file number
              integer         lsf_num
C         Command line prompt and length.
              character*40    prompt
              integer         lp
              data            prompt, lp / 'Monitor> ', 10 /
C         String length
              integer         ls

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
          call io_getcmd(prompt(1:lp), cmd_list, num_cmds, command, s)
          if (command.eq.0) goto 1000

          if (s .ne. 0) then
              continue
          else if ( command .eq. set_sample_file_cmd ) then
              new_sf  = ' '
              new_lsf = -1
              call lsf_open( new_sf, new_lsf, 'READ', lsf_num, s )
              call lsf_close( lsf_num, s )
              if ( s .eq. 0 ) then
                  def_sf  = new_sf
                  def_lsf = new_lsf
                  call io_wrout( ' ' )
                  call io_wrout( def_sf )
                  call io_wrout( ' ' )
              end if
          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call lsf_open( def_sf, new_lsf, 'READ', lsf_num, s )
              call lsf_close( lsf_num, s )
              if ( s .eq. 0 ) def_lsf = new_lsf
              if ( s .eq. NO_LSFSAVED ) then
                  s = 0
                  call io_wrout( 'No logical sample files are saved.' )
              end if
          else if ( command .eq. anal_holog_cmd ) then
              call lsf_open( def_sf, def_lsf, 'READ', lsf_num, s )
              call anal_holog_ryle( lsf_num, plot_device, s )
              call lsf_close( lsf_num, s )
          else if ( command .eq. anal_point_cmd ) then
              call lsf_open( def_sf, def_lsf, 'READ', lsf_num, s )
              call anal_point_ryle( lsf_num, plot_device, s )
              call lsf_close( lsf_num, s )
          else if ( command .eq. apply_patch_cmd ) then
              call apply_patch_ryle( def_sf, s )
          else if ( command .eq. check_alc_cmd ) then
              call check_alcs_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_cryo_cmd ) then
              call check_cryo_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_misc_cmd ) then
              call check_misc_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_phe_cmd ) then
              call check_phe_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_point_cmd ) then
              call check_point_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_rain_cmd ) then
              call check_rain_gauge( def_sf, plot_device, s )
          else if ( command .eq. check_vac_cmd ) then
              call check_vac_ryle( def_sf, plot_device, s )
          else if ( command .eq. check_wind_cmd ) then
              call check_wind_gauge( def_sf, plot_device, s )
c         else if ( command .eq. copy_sample_file_cmd ) then
c             call copy_samp_ryle( def_sf, s )
          else if ( command .eq. dump_spacings_cmd ) then
              call dump_spacings( def_sf, s )
          else if ( command .eq. flag_point_cmd ) then
              call flag_point_ryle( def_sf, def_lsf, s )
CR        else if ( command .eq. log_comment_cmd ) then
CR            call io_wrout(
CR   :                'Type your comment to the RYLE observation log' )
CR            call log_comment(observation_log, s )
CR            call io_wrout( ' ' )
          else if ( command .eq. print_config_cmd ) then
              call print_config_ryle( def_sf, s )
          else if ( command .eq. print_geometry_cmd ) then
              call print_geometry( def_sf, s )
          else if ( command .eq. print_obs_cmd ) then
              call print_obs_pars( def_sf, s )
          else if ( command .eq. print_patch_cmd ) then
              call print_patch_ryle( def_sf, s )
          else if ( command .eq. recover_samp_cmd ) then
              call recover_samp_file( def_sf, s )
          else if ( command .eq. reset_samp_count_cmd ) then
              call reset_samp_count( def_sf, s )
c         else if ( command .eq. save_sample_file_cmd ) then
c             call save_samp_ryle( def_sf, s )
          else if ( command .eq. set_comment_cmd ) then
              call set_comment_ryle( def_sf, s )
          else
              ls = chr_lend( cmd_list(command), '.' )
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')
          end if

C         Restore status.
          if ( s .eq. usr_break ) s = 0
          if ( s .ne. 0 ) goto 9999

 1000 continue

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call mon_wrerr( s, 'in subroutine MON_SYS_RYLE' )
          return
      end

