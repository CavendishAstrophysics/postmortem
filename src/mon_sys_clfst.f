C
C+mon_sys_clfst
C
      SUBROUTINE mon_sys_clfst ( def_sf, def_lsf, command, s )

C
C     The monitor system within postmortem.  CLFST version.
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
C     Combines all the CLFST monitor system commands in one place.
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
          include '/mrao/post/include/mon_comms_clfst.inc'
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
  100 continue
          call cmd_getcmd(prompt(1:lp), cmd_list, num_cmds, command, s)
          if (command.eq.0) goto 1000

          if (s .ne. 0) then
              continue
          else if (command .le. 0) then
              call basic_commands( command, prompt, lp, s )
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
          else if ( command .eq. check_agcs_cmd ) then
              call check_agcs_clfst( def_sf, plot_device, s )
          else if ( command .eq. check_corr_cmd ) then
              call check_corr_clfst( def_sf, s )
          else if ( command .eq. check_inter_cmd ) then
              call check_inter_clfst( def_sf, plot_device, s )
          else if ( command .eq. check_pcs_cmd ) then
              call check_pcs_clfst( def_sf, plot_device, s )
          else if ( command .eq. check_pointing_cmd ) then
              call check_point_clfst( def_sf, plot_device, s )
          else if ( command .eq. dump_spacings_cmd ) then
              call dump_spacings( def_sf, s )
          else if ( command .eq. list_spacings_cmd ) then
              call list_spac_clfst( def_sf, s )
          else if ( command .eq. print_aerial_status_cmd ) then
              call print_aes_clfst( def_sf, s )
          else if ( command .eq. print_ampf_cmd ) then
              call print_ampf_clfst( def_sf, s )
          else if ( command .eq. print_coll_cmd ) then
              call print_coll_clfst( def_sf, s )
          else if ( command .eq. print_control_tables_cmd ) then
              call print_ct_clfst( def_sf, s )
          else if ( command .eq. print_hut_status_cmd ) then
              call print_huts_clfst( def_sf, s )
          else if ( command .eq. print_geometry_cmd ) then
              call print_geometry( def_sf, s )
          else if ( command .eq. print_obs_cmd ) then
              call print_obs_pars( def_sf, s )
          else if ( command .eq. print_sidereal_time_cmd ) then
              call print_sid_time( def_sf, s )
          else if ( command .eq. print_zero_corr_cmd ) then
              call print_zeros_clfst( def_sf, s )
          else if ( command .eq. recover_samp_cmd ) then
              call recover_samp_file( def_sf, s )
          else if ( command .eq. reset_samp_count_cmd ) then
              call reset_samp_count( def_sf, s )
          else if ( command .eq. save_sample_file_cmd ) then
c             call save_samp_clfst( def_sf, s )
          else if ( command .eq. scan_aerials_cmd ) then
c             call lsf_open( def_sf, def_lsf, 'READ', lsf_num, s )
c             call scan_aes_clfst( lsf_num, s )
c             call lsf_close( lsf_num, s )
          else if ( command .eq. scan_sample_file_cmd ) then
              call scan_sf_clfst( def_sf, s )
          else
              ls = chr_lend( cmd_list(command), '.' )
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')
          end if

C         Restore status.
          if ( s .eq. usr_break ) s = 0
          if ( s .ne. 0 ) goto 9999
          if (command .ne. exit_cmd .and.
     *        command .ne. set_sample_file_cmd ) goto 100

 1000 continue

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call mon_wrerr( s, 'in subroutine MON_SYS_CLFST' )
          return
      end
