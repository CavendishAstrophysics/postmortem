C
C Control record for solution of aerial/visibility gains
C
       integer                    cal_soln_record(40)
       common /cal_soln_control/  cal_soln_record

C Parameters controlling accuracy of the solution
C   tolerance in aerial matrix elements
       real*8        atol
       equivalence  (atol, cal_soln_record(1))
C   tolerance in visibility phases
       real*8        btol_phase
       equivalence  (btol_phase, cal_soln_record(3))
C   tolerance in visibility amplitudes
       real*8        btol_amp
       equivalence  (btol_amp, cal_soln_record(5))
C   control of printing information from NAG routine
       integer       icontrol
       equivalence  (icontrol, cal_soln_record(7))
C   information returned from fitting routine (1=phase, 2=amplitude)
       integer       its_done(2), inform(2)
       equivalence  (its_done(1), cal_soln_record(8))
       equivalence  (inform(1), cal_soln_record(10))
       real*8        anorm(2), acond(2), rnorm(2), arnorm(2), xnorm(2)
       equivalence  (anorm(1), cal_soln_record(13))
       equivalence  (acond(1), cal_soln_record(17))
       equivalence  (rnorm(1), cal_soln_record(21))
       equivalence  (arnorm(1), cal_soln_record(25))
       equivalence  (xnorm(1), cal_soln_record(29))
C   control of phase fiddling
       logical       no_fiddle
       equivalence  (no_fiddle, cal_soln_record(33))
C   control of printing closure phases
       integer       print_closure
       equivalence  (print_closure, cal_soln_record(34))
C   control printing of the report information from NAG routine
       integer       print_report
       equivalence  (print_report, cal_soln_record(35))
C   current logical aerial definition
       integer       current_subband
       integer       current_channel
       equivalence  (current_subband, cal_soln_record(36))
       equivalence  (current_channel, cal_soln_record(37))
C   gate level for closure phase error reporting (degrees)
       real*4        closure_gate
       equivalence  (closure_gate, cal_soln_record(38))

C
C Options for the GT system
C   control reporting the record written to the GT file
       logical             gtopt_report_write
C   place these in common
       common /cal_gtopts/ gtopt_report_write
C   files
       character*64        RT_gt_file
       character*64        RT_gtvis_file
       common /cal_gtfiles/ RT_gt_file, RT_gtvis_file
