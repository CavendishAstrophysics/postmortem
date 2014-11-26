C
C+remove_system
C
      SUBROUTINE remove_system( def_sf, def_lsf, s )

C
C     The remove system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current lsf key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the remove system functions in one place.
C
C-
C     ==================================================================
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/remsys_errors.inc'
          include '/mrao/post/include/remsys_commands.inc'
          include '/mrao/post/include/post_common.inc'
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'

C     Variables, equivalences and commons
C         Current command.
              integer         command
C         General purpose string and length
              character*80    string
              integer         ls
C         Execution envirionment parameters.
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
              data            prompt, lp / 'Remove> ', 8 /

C
C Subroutine initialisation
C -------------------------
C
      if ( s .ne. 0 ) return

      call io_wrout( ' ' )
      call io_wrout( def_sf )
      call rem_init_remove( def_sf, def_lsf, s )
      call io_wrout( ' ' )
      if ( s .ne. 0 ) goto 9999

      call io_enqexe( user, mode, termno )

C
C Main Code
C ---------
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
              call rem_init_remove( file_name, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_sf  = file_name
                  def_lsf = new_lsf
                  call io_wrout( ' ' )
                  call io_wrout( def_sf )
                  call io_wrout( ' ' )
              end if

          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call rem_init_remove( def_sf, new_lsf, s )
              if ( s .eq. 0 ) then
                  def_lsf = new_lsf
              else if ( s .eq. NO_LSFSAVED ) then
                  s = 0
                  call io_wrout( 'No logical sample files are saved.' )
              end if

          else if ( command .eq. open_rem_cmd ) then
              call rem_open( def_sf, def_lsf, s )

          else if ( command .eq. save_rem_cmd ) then
              call rem_save( def_sf, s )

          else if ( command .eq. display_rem_cmd ) then
              call rem_display( def_sf, s )

          else if ( command .eq. model_rem_cmd ) then
              call rem_sel_model( s )

          else if ( command .eq. make_rem_cmd ) then
              call open_sf( lun, def_sf, 'READ', 0, s )
              call enq_namfil( lun, 'REM', file_name, s )
              if ( s .eq. NO_FILE ) then
                  s = 0
                  call io_wrout( 'Please save a remove first...' )
                  call close_sf( lun, s )
              else
                  call close_sf( lun, s )
                  call io_system( 'setenv SAMPDIR '//def_dir, s )
                  call io_system( '/mrao/bin/remove '//def_sf//' &', s )
              end if

          else if ( command .eq. interactive_rem_cmd ) then
              call open_sf( lun, def_sf, 'READ', 0, s )
              call enq_namfil( lun, 'REM', file_name, s )
              if ( s .eq. NO_FILE ) then
                  s = 0
                  call io_wrout( 'Please save a remove first...' )
                  call close_sf( lun, s )
              else
                  call close_sf( lun, s )
                  call rem_make_remove( file_name, s )
              end if

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

C
C Eror Handling
C -------------
C
 9999 continue
          call rem_wrerr( s, 'in subroutine REMOVE_SYSTEM' )
          return
      end
