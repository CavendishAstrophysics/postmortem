C*
      PROGRAM postmortem
C     ------------------
C
C     Cambridge Synthesis Telescopes Observation Reduction Package
C
C     Version 3.0
C
C     Telescopes supported:   CLFST (151MHz, 38MHz)
C                             RYLE  (5GHz, 15GHz)
C                             VLBI  (81.5MHz)
C
C     N.P. Rees,
C     P. Alexander,
C     D.J. Titterington
C     P.J. Warner
C last change 2 June 2000 GP
C*
C ======================================================================
C
C Local constant and variable declarations
C
C     Constants
          include '/mrao/post/include/global_constants.inc'
          include '/mrao/post/include/post_common.inc'
          include '/mrao/post/include/post_sys_pars.inc'
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/post_commands.inc'
          include '/mrao/post/include/basic_commands.inc'

C     Variables, equivalences and commons
C         Status variable and string length
              integer         s, ls
C         Sample file names.
              character*(80)  def_sf, new_sf
              integer         sf_lun
C         Exit program name
              character*80    exit_program
              integer         len_program
C         EXIST flag for initialisation file
              logical         exist
C         LSF numbers and keys.
              integer         def_lsf, new_lsf, lsf_num
C         Command line on startup of POSTMORTEM
              character*120   command_buffer
              integer         len_com_buffer
C         Current command
              integer         command
C         Current input device on entry
              integer         indev
C         Command line prompt and length.
              character*40    prompt
              integer         lp

                logical         test_version
                parameter      (test_version = .FALSE.)
C
C

C     Make work space available
               include '/mrao/post/include/post_work_array.inc'

C
C
C Program initialisation
C ----------------------
C
C initialise for use of IOLIB and command language
      s = 0
      call io_initio
      call cmd_init(s)
      call cmd_setscope(2)

      call io_enqin(indev)
      call io_setesc(.true.)
      call io_setexc(.true., s)
c     call io_setlog(error_file, s)

C define basic commands
      call cmd_defcmd(basic_cmd_list, num_basic_cmds, s)
      call cmd_scncmd(.true.)

C define defaults
      def_sf  = ' '
      def_lsf = -1
      call getenv('SAMPDIR', def_dir)

C define default plot device
      call io_enqplt(0, plot_device)
      if (s .ne. 0) goto 9999

C
C
C Main Code
C ---------

C set banner display control parameter
       call io_enqcli(command_buffer, len_com_buffer)
       if (len_com_buffer.gt.0) then
         call cmd_setparam('%display-banner','F',s)
       else
         call cmd_setparam('%display-banner','T',s)
       end if

C execute the initialisation file
       inquire (file=initialisation_file, exist=exist)
       if (exist) then
C .. user initialisation file
         call cmd_runfile(initialisation_file,s)
       else
C .. system initialisation file and prompt
         if (test_version) then
                lp = 11
                prompt = 'Test-post> '
                call cmd_runfile(system_test,s)
         else
                lp =  6
                prompt = 'Post> '
                call cmd_runfile(system_init,s)
         endif
       end if

  100 continue
          call cmd_setparam('%sub-system', 'POST', s)
          call cmd_getcmd(prompt(1:lp), cmd_list, num_cmds, command, s)

          if (s .ne. 0) then
              continue

          else if (command .le. 0) then
              call basic_commands(command, prompt, lp, s)

          else if (command .eq. set_sample_file_cmd) then
              new_sf  = ' '
              new_lsf = -1
              call open_sf (sf_lun, new_sf, 'READ', 0, s)
              if (s .eq. 0) then
                 call close_sf(sf_lun, s)
                 def_sf = new_sf
                 def_lsf = -1
                 call io_wrout(' ')
                 call io_wrout(def_sf)
                 call io_wrout(' ')
                 call lsf_open(def_sf, def_lsf, 'READ', lsf_num, s)
                 call lsf_close(lsf_num, s)
              end if

          else if (command .eq. set_lsf_cmd) then
              new_lsf = 0
              call lsf_open(def_sf, new_lsf, 'READ', lsf_num, s)
              call lsf_close(lsf_num, s)
              if (s .eq. 0) then
                 def_lsf = new_lsf
              elseif (s .eq. NO_LSFSAVED) then
                 call io_wrout('No logical sample files are saved.')
                 s = 0
              end if

          else if (command .eq. print_obs_cmd) then
              call print_obs_pars(def_sf, s)

          else if (command .eq. lsf_system_cmd) then
              call cmd_setparam('%sub-system','LSF',s)
              call lsf_system(def_sf, def_lsf, s)

          else if (command .eq. monitor_system_cmd) then
              call cmd_setparam('%sub-system','MONITOR',s)
              call monitor_system(def_sf, def_lsf, s)

          else if (command .eq. mapping_system_cmd) then
              call cmd_setparam('%sub-system','MAPPING',s)
              call mapping_system(def_sf, def_lsf, s)

          else if (command .eq. remove_system_cmd) then
              call cmd_setparam('%sub-system','REMOVE',s)
              call remove_system(def_sf, def_lsf, s)

          else if (command .eq. calib_system_cmd) then
              call cmd_setparam('%sub-system','CALIBRATION',s)
              call calib_system(def_sf, def_lsf, s)

          else if (command .eq. ion_system_cmd) then
              call cmd_setparam('%sub-system','ION',s)
              call ion_system(def_sf, def_lsf, s)

          else
              ls = chr_lend(cmd_list(command), '.')
              call io_wrout(
     *          'Command '//cmd_list(command)(1:ls)//'not implemented.')

          end if

C Restore status.
          if (s .eq. usr_break) s = 0
          if (s .ne. 0) goto 9999

      if (command .ne. 0) goto 100

 1000 call io_wrout(' ')

*      call cmd_comm_rdclo(s)
      call io_setin(indev)
      call io_setesc(.false.)
C find exit program if any and execute command
      call cmd_enqparam('%exit-program', exit_program, s)
      call cmd_unsetparam('%exit-program', s)
      call cmd_end(s)
      len_program = chr_lenb(exit_program)
      if (len_program.gt.0) then
         call io_system(exit_program(1:len_program), s)
      end if

C
C Error Handling
C --------------
C
 9999 if (s.ne.0) then
          call io_wrerr(s, 'in program V2-POSTMORTEM')
          s = 0
          call io_wrout('*** Please report this error')
          goto 100
      end if

      end
