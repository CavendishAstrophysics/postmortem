C$ Routines for Monitoring the solution to RT calibration
C
C
C P. Alexander, MRAO, Cambridge.
C
C+ cal_soln_monitor

       subroutine cal_soln_monitor( s )
C      --------------------------------
C
C Provide monitor functions for the calibration solution and GT system
C
C Returned:
C    status variable
       integer          s
C
C Interactive sub-system providing access to monitor commands
C
C PA, 22/01/92
C-

C setup system-wide definitions
       include '/mrao/post/include/post_sys_pars.inc'
C define iolib/chrlib functions
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C include control information
       include '/mrao/post/include/cal_control.inc'
C define commands available in the solution monitor
       integer       number_commands, icomm
       logical       exit_at_end
       parameter    (number_commands = 9)
       character*80  liscom(number_commands), command_line
       integer      cmd_report, len_cli, il
       parameter   (cmd_report = 1)
       data liscom(cmd_report)/
     * 'enable-report ....... enable/disable reports on the solution'
     *               /
       integer      cmd_closure
       parameter   (cmd_closure = 2)
       data liscom(cmd_closure)/
     * 'enable-closure ...... enable/disable printing of closure phases'
     *               /
       integer      cmd_NAG
       parameter   (cmd_NAG = 3)
       data liscom(cmd_NAG)/
     * 'enable-NAG .......... enable/disable reports from NAG routine'
     *               /
       integer      cmd_print_rep
       parameter   (cmd_print_rep = 4)
       data liscom(cmd_print_rep)/
     * 'print-report ........ print report of last solution'
     *               /
       integer      cmd_phase_fiddle
       parameter   (cmd_phase_fiddle = 5)
       data liscom(cmd_phase_fiddle)/
     * 'set-phase-fiddle .... turn phase fiddle on/off'
     *               /
       integer      cmd_set_tol
       parameter   (cmd_set_tol = 6)
       data liscom(cmd_set_tol)/
     * 'set-tolerances ...... set tolerances for the solution'
     *               /
       integer      cmd_check_closure
       parameter   (cmd_check_closure = 7)
       data liscom(cmd_check_closure) /
     * 'check-closure-phase . enable reporting of large closure errors'
     *               /
       integer      cmd_report_write
       parameter   (cmd_report_write = 8)
       data liscom( cmd_report_write ) /
     * 'report-gt-write ..... toggle reporting of GT record written'
     *               /
       integer      cmd_set_gt_file
       parameter   (cmd_set_gt_file = 9)
       data liscom( cmd_set_gt_file ) /
     * 'set-gt-files ........ define names of GT and GTvis files'
     *               /


C check status on entry
       if (s.ne.0) return

C provide loop
       call io_enqcli(command_line,len_cli)
       exit_at_end = len_cli.ne.0
100    s=0
       call cmd_scncmd( .false.)
       call cmd_getcmd('Solution-Monitor> ',liscom,
     *                 number_commands, icomm, s)
       call cmd_err( s, 'SOLUTION-MONITOR', 'in cal_soln_monitor' )
       call cmd_scncmd( .true. )

C decode command
       if (icomm.eq.0) return

       if (icomm.eq.cmd_report) then
         if (io_yesno('Report solution results : ','no',s)) then
           print_report = 1
         else
           print_report = 0
         end if

       else if (icomm.eq.cmd_closure) then
         if (io_yesno('Report closure phases : ','no',s)) then
           print_closure = 1
         else
           print_closure = 0
         end if

       else if (icomm.eq.cmd_NAG) then
         if (io_yesno('Report from NAG routine : ','no',s)) then
           icontrol = 1
         else
           icontrol = 0
         end if

       else if (icomm.eq.cmd_print_rep) then
         call cal_soln_report( 1, s )

       else if (icomm.eq.cmd_phase_fiddle) then
         if (io_yesno(
     *           'Fiddle-phases prior to solution : ','yes',s)) then
           no_fiddle = .false.
         else
           no_fiddle = .true.
         end if

       else if (icomm.eq.cmd_set_tol) then
         call io_getd('Tolerance in matrix (ATOL) : ', '0.0', atol, s)
         call io_getd('Tolerance in phase (BTOL) : ', '0.0',
     *             btol_phase, s )
         call io_getd('Tolerance in amplitude (BTOL) : ', '0.0',
     *             btol_amp, s )

       else if (icomm.eq.cmd_check_closure) then
         call io_getr('Warning-level for closure phase (degrees) : ',
     *             '10.0', closure_gate, s)
         if (s.eq.0) then
           print_closure = -1
         else
           print_closure = 0
         end if

       else if (icomm.eq.cmd_report_write) then
         gtopt_report_write = 
     *             io_onoff('Turn reporting on/off : ','on',s)

       else if (icomm.eq.cmd_set_gt_file) then
         if (chr_lenb(RT_gt_file).eq.0) then
           RT_gt_file = def_RT_gt_file
         end if
         if (chr_lenb(RT_gtvis_file).eq.0) then
           RT_gtvis_file = def_RT_gtvis_file
         end if
         call io_getwrd('GT-file : ',RT_gt_file,RT_gt_file,il,s)
         call io_getwrd('GTvis-file : ',RT_gtvis_file,RT_gtvis_file,
     *                                                           il,s)

       end if

       if (.not.exit_at_end) goto 100

       end
