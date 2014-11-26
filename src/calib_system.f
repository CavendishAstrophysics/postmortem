C
C+calib_system
C
      SUBROUTINE calib_system( def_sf, def_lsf, s )

C
C     The calibration system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current LSF key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C last changed 25 June 2001 (undelete-gt-record)
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/post/include/global_constants.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/calib_errors.inc'
          include '/mrao/post/include/calib_commands.inc'
          include '/mrao/post/include/cal_solution.inc'
          include '/mrao/post/include/cal_control.inc'
          include '/mrao/post/include/post_common.inc'

C     Variables, equivalences and commons
C         Current command.
              integer         command
C         General purpose string and length
              character*80    string
              integer         ls
C         Execution environment parameters.
              integer         mode, termno
              character*16    user
c             integer         i4_values(10)
c             real*4          r4_values(10)
c             real*8          r8_values(10)
C         General purpose file name and unit number.
              character*80    file_name
              integer         lun
C         New lsf key
              integer         new_lsf
C         Command line prompt and length.
              character*40    prompt
              integer         lp
              data            prompt, lp / 'Calibration> ', 13 /
C         Number of command actually current
              integer         ncom_current

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
      if ( s .ne. 0 ) return

      call io_wrout( ' ' )
      call io_wrout( def_sf )
      call cal_init_calib( def_sf, def_lsf, ncom_current, s )
      call cal_gt_init( s )
      call io_wrout( ' ' )
      if ( s .ne. 0 ) goto 9999

      call io_enqexe( user, mode, termno )

C
C     Main Code
C     ---------
C
C     Initialise default value for GT write reporting
C
      gtopt_report_write = .true.


  100 continue
          call cmd_getcmd(prompt(1:lp), cmd_list, ncom_current,
     *                    command, s)

          if (s .ne. 0) then
              continue

          else if (command .le. 0) then
              call basic_commands( command, prompt, lp, s )

          else if ( command .eq. set_sample_file_cmd ) then
              file_name  = ' '
              new_lsf = -1
              call cal_init_calib( file_name, new_lsf, ncom_current, s )
              if ( s .eq. 0 ) then
                  def_sf  = file_name
                  def_lsf = new_lsf
                  call io_wrout( ' ' )
                  call io_wrout( def_sf )
                  call io_wrout( ' ' )
              end if

          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call cal_init_calib( def_sf, new_lsf, ncom_current, s )
              if ( s .eq. 0 ) then
                def_lsf = new_lsf
              end if

          else if ( command .eq. open_cal_cmd ) then
              call cal_open( def_sf, def_lsf, s )

          else if ( command .eq. save_cal_cmd ) then
              call cal_save( def_sf, s )

          else if ( command .eq. display_cal_cmd ) then
              call cal_display( def_sf, s )

          else if ( command .eq. select_model_cmd ) then
              call cal_sel_model( s )

          else if ( command .eq. select_amp_cmd ) then
              call cal_sel_ampsol( s )

          else if ( command .eq. select_phase_cmd ) then
              call cal_sel_phisol( s )

          else if ( command .eq. select_calib_cmd ) then
              call cal_sel_calib( def_sf, s )

          else if ( command .eq. select_bandwidth_cmd ) then
              call cal_sel_bandwid( def_sf, s )

          else if ( command .eq. select_refant_cmd ) then
              call cal_sel_refant( def_sf, s )

          else if ( command .eq. soln_monitor_cmd ) then
              call cal_soln_monitor( s )

          else if ( command .eq. display_sample_cmd ) then
              call cal_disp_sample( plot_device, s )

          else if ( command .eq. display_spacing_cmd ) then
              call cal_disp_spacing( plot_device, s )

          else if ( command .eq. make_cal_cmd ) then
              call open_sf( lun, def_sf, 'READ', 0, s )
              call enq_namfil( lun, 'CAL', file_name, s )
              if ( s .eq. NO_FILE ) then
                  s = 0
                  call io_wrout( 'Please save a calibration first...' )
                  call close_sf( lun, s )
              else
                  call close_sf( lun, s )
                  call io_system( 'setenv SAMPDIR '//def_dir, s )
                  call io_system(
     *                 '/mrao/bin/calibration '//def_sf//' &', s )
              end if

          else if ( command .eq. interactive_cal_cmd ) then
              call open_sf( lun, def_sf, 'READ', 0, s )
              call enq_namfil( lun, 'CAL', file_name, s )
              if ( s .eq. NO_FILE ) then
                  s = 0
                  call io_wrout( 'Please save a calibration first...' )
                  call close_sf( lun, s )
              else
                  call close_sf( lun, s )
                  call cal_make_calib( file_name, s )
              end if

          else if ( command .eq. gt_write_cmd ) then
              call cal_gt_write( def_sf, s )

          else if ( command .eq. gt_delete_cmd ) then
              call cal_gt_delete( s )

          else if ( command .eq. gt_examine_cmd ) then
              call cal_gt_examine( def_sf, s )

          else if ( command .eq. gt_update_cmd ) then
              call cal_gt_update( s )

          else if ( command .eq. gt_display_cmd ) then
              call cal_gt_display( ae_gains, plot_device, s )

          else if ( command .eq. gtvis_display_cmd ) then
              call cal_gtvis_display( vis_gains, plot_device, s )

          else if ( command .eq. gt_apply_cmd ) then
              call cal_gt_apply( def_sf, s )

          else if ( command .eq. gt_vissol_cmd ) then
              call cal_sel_vissol( s )

          else if (command .eq. cal_new_geom_cmd) then
                call cal_new_geom(def_sf, s)

          else if (command .eq. gt_undelete_cmd) then
                call cal_gt_undelete(s)

          else
              ls = chr_lend( cmd_list(command), '.' )
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')

          end if

C         Restore status.
          if ( s .eq. NO_LSFSAVED ) then
             call io_wrout( '*** No logical sample files are saved' )
             s = 0
          end if
          if ( s .eq. usr_break ) s = 0
          if ( s .ne. 0 ) goto 9999

      if ( command .ne. exit_cmd ) goto 100

      call io_enqcli( string, ls )
      if ( ls.eq.0 ) call io_wrout( ' ' )

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr( s, 'in subroutine calib_system.' )
          return
      end
