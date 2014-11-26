*$ Basic Commands for POSTMORTEM
*  ----------------------------
C
C Basic Commands are commands available at all levels of POSTMORTEM
C

C
C+basic_commands
C
      SUBROUTINE basic_commands(  command,
     *                            prompt, lp,
     *                            s              )

C
C     Executes the basic commands in the Postmortem System.
C
C     Given:
C         Integer code of command to execute.
              integer             command
C         Current prompt and length
              character*(*)       prompt
              integer             lp

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the basic command functions in one place.
C
C-
C     ==================================================================
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/post/include/basic_commands.inc'
          include '/mrao/post/include/post_common.inc'
          include '/mrao/post/include/post_sys_pars.inc'
          include '/mrao/post/include/post_work_array.inc'

C         General purpose string and length
              character*80    string
              character*120   command_line
              integer         ls, sid_time(3), len_cli, len_string
              integer         i1
C         Output unit
              integer         iout
C         Current redtape save buffer
              integer         buffer(512)

C
C Subroutine initialisation
C -------------------------

      if ( s .ne. 0 ) return

C
C Main Code
C ---------
C
C     Process commands.
      command = abs( command )
      if ( command .eq. 0 .or. command .eq. exit_cmd ) then
          command = 0

      else if ( command .eq. list_basic_cmd ) then
          call io_prtopt(basic_cmd_list, num_basic_cmds, ' ', s)

      else if ( command .eq. set_dir_cmd ) then
          call io_getfil('Sample file directory : ', '*', def_dir, s)

      else if ( command .eq. set_plot_cmd ) then
          call io_getplt('Plot-device : ', '*', plot_device, s)

      else if ( command .eq. sidereal_time_cmd ) then
          call util_enqsid(sid_time)
          call chr_chtime(sid_time, string, ls)
          call io_wrout('Sidereal time now is  '//string(1:ls)//' ST')
          call io_wrout( ' ' )

      else if ( command .eq. examine_sample_cmd ) then
          string = ' '
          call io_enqcli( string, ls )
          call ex_samp( string(1:ls), s )

      else if ( command .eq. examine_maps_cmd ) then
          call dpredt(buffer, s)
          string = ' '
          call io_enqcli( string, ls )
          call ex_maps( string(1:ls), s )
          call ldredt(buffer, s)

      else if ( command .eq. examine_lsf_cmd ) then
          string = ' '
          call io_enqcli( string, ls )
c         call ex_lsf( string(1:ls), s )

      else if ( command .eq. find_source_cmd ) then
          call find_sources( s )

      else if ( command .eq. help_cmd ) then
          call hlp_setfil( help_file, s )
          call io_enqcli( command_line, len_cli )
          i1 = chr_intlc( command_line(1:len_cli) )
          if (command_line(i1:i1).ne.'@') then
c           if (len_cli.gt.0) then
              call cmd_enqparam( '%sub-system', string, s )
              len_string = chr_lenb(string)
              if (string(1:len_string).ne.'POST') then
                string(len_string+1:) = ' '//command_line(1:len_cli)
                command_line = string
                len_cli = chr_lenb( command_line )
              end if
c           end if
          end if
          call hlp_system( command_line(1:len_cli),
     *                     'TERMINAL', .true., .true., s)
          call cmd_err(s,'HELP',' ')
          call io_wrout( ' ' )

      else if ( command .eq. version_cmd ) then
          call io_enqout(iout)
          write(iout,'(1X,A,A,A,A)')
     *     'POSTMORTEM Version ',post_version,' of ',post_date

      else if ( command .eq. banner_cmd ) then
          call io_enqout(iout)
          call io_prtfil( iout, banner_file, s)

      else if ( command .eq. filesys_cmd ) then
*         call cmd_filesys( ' ', 'File> ', s )

      else if ( command .eq. batchsys_cmd ) then
*         call batch_monitor( s )

      else if ( command .eq. disopts_cmd ) then
          call plot_getopts( plot_device, s )

      else if ( command .eq. metafile_cmd ) then
*         call plot_metafile( s )

      else if ( command .eq. news_cmd ) then
          call hlp_setfil( news_file, s)
          call hlp_system( ' ', ' ', .true., .true., s)
          call io_wrout( ' ' )

      else if ( command .eq. lsf_sys_cmd .or.
     *          command .eq. mon_sys_cmd .or.
     *          command .eq. map_sys_cmd .or.
     *          command .eq. cal_sys_cmd .or.
     *          command .eq. ion_sys_cmd .or.
     *          command .eq. rem_sys_cmd ) then
          call io_enqcli(string,len_string)
          ls = chr_lenw(basic_cmd_list(command))
          command_line = basic_cmd_list(command)(1:ls)//' '//
     *                                              string(1:len_string)
          len_cli = chr_lenb(command_line)
          call io_setcli(command_line(1:len_cli))
          command = 0

      else
          ls = chr_lenw(basic_cmd_list(command))
          call io_wrout(
     *   'Command '//basic_cmd_list(command)(1:ls)//' not implemented.')

      end if

      if (s.ne.0) goto 9999
      return

C
C Error Handling
C --------------
C
 9999 continue
          if (s.ne.USR_BREAK) then
              call io_wrerr( s, 'in subroutine basic_commands' )
          end if
          return
      end
