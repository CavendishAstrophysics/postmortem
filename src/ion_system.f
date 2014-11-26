C
C+ion_system
C
      subroutine ion_system( def_sf, def_lsf, s )
C
C     The ionospheric correction system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current logical sample file key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the ionospheric correction system functions
C     in one place.
C
C-
C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/ion_commands.inc'
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
c             integer         i4_values(20)
c             real*4          r4_values(20)
c             real*8          r8_values(20)
C         General purpose file name and unit number.
              character*80    file_name
              integer         lun
C         New lsf key
              integer         new_lsf
C         Command line prompt and length.
              character*40    prompt
              integer         lp
              data            prompt, lp / 'Ion> ', 5 /

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

      call io_wrout( ' ' )
      call io_wrout( def_sf )
      call ion_init_ion( def_sf, def_lsf, s )
      call io_wrout( ' ' )
      if ( s .ne. 0 ) goto 9999

      call io_enqexe( user, mode, termno )

C     ****************************************************************
C
C         Main Code
C         ---------
C

  100 continue
          call cmd_getcmd(prompt(1:lp), cmd_list, num_cmds, command, s)

          if (s .ne. 0) then
              continue
          else if (command .le. 0) then
              call basic_commands( command, prompt, lp, s )
          else if ( command .eq. set_sample_file_cmd ) then
              file_name  = ' '
              new_lsf = -1
              call ion_init_ion( file_name, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_sf  = file_name
                  def_lsf = new_lsf
                  call io_wrout( ' ' )
                  call io_wrout( def_sf )
                  call io_wrout( ' ' )
              end if
          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call ion_init_ion( def_sf, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_lsf = new_lsf
              else if ( s .eq. NO_LSFSAVED ) then
                  s = 0
                  call io_wrout( 'No logical sample files are saved.' )
              end if
          else if ( command .eq. save_ion_cmd ) then
              call ion_save( def_sf, s )
          else if ( command .eq. open_ion_cmd ) then
              call ion_open( def_sf, def_lsf, s )
          else if ( command .eq. interactive_ion_cmd ) then
              call ion_make_corr( def_sf, s )
          else if ( command .eq. make_ion_cmd ) then
              call open_sf( lun, def_sf, 'READ', 0, s )
              call enq_namfil( lun, 'ION', file_name, s )
              if ( s .eq. NO_FILE ) then
                  s = 0
                  call io_wrout( 'Please save a correction first...' )
                  call close_sf( lun, s )
              else
                  call close_sf( lun, s )
                  call io_system( 'setenv SAMPDIR '//def_dir, s )
                  call io_system(
     *              '/mrao/bin/ion_correction '//def_sf//' &', s )
              end if
          else if ( command .eq. display_ion_corr_cmd ) then
              call ion_disp_corr( def_sf, plot_device, s )
          else if ( command .eq. edit_ion_corr_cmd ) then
              call ion_edit_corr( def_sf, plot_device, s )
          else if ( command .eq. display_ion_samp_cmd ) then
              call ion_disp_samp( def_sf, plot_device, s )
          else if ( command .eq. print_all_cmd ) then
              call ion_list_corrs( def_sf, s )
          else if ( command .eq. print_correction_cmd ) then
              call ion_print( def_sf, s )
          else if ( command .eq. add_source_cmd ) then
              call ion_add_source( s )
          else if ( command .eq. delete_source_cmd ) then
              call ion_del_source( s )
          else if ( command .eq. change_parameters_cmd ) then
              call ion_change_params( def_sf, s )
          else if ( command .eq. display_ion_spectrum_cmd ) then
              call ion_disp_spect( def_sf, plot_device, s )
          else if ( command .eq. display_wavefront_cmd ) then
              call ion_disp_wavefront( def_sf, plot_device, s )
          else
              ls = chr_lend( cmd_list(command), '.' )
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')
          end if

C         Restore status.
          if ( s .eq. usr_break ) s = 0
          if ( s .ne. 0 ) goto 9999
      if ( command .ne. exit_cmd ) goto 100

      call io_enqcli( string, ls )
      if ( ls.eq.0 ) call io_wrout( ' ' )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_system.' )
          return
      end
