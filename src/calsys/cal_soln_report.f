C
C
C+ cal_soln_report

       subroutine cal_soln_report( iprint, s )
C      ---------------------------------------
C
C Report details of the gains solution
C
C Given:
C   printing control parameter
       integer    iprint
C Returned:
C   status flag
       integer    s
C
C Report details of the gains solution if iprint is non-zero.  This
C routine is most useful for debugging purposes.
C
C-
C PA 10/4/90
C
C Include gains control information
      include '/mrao/post/include/cal_control.inc'
C local variables:
C   output unit number
       integer     iout


C test status and printing flag on entry
       if (s.ne.0 .or. iprint.eq.0) return

C find output unit number
       call io_enqout(iout)

C report current default settings
       write (iout,10) atol, btol_phase, btol_amp, icontrol,
     *                 print_closure, print_report, no_fiddle
10     format(1x/
     *        1x,'Report:   Parameters and results of solution'/
     *        1x,'--------------------------------------------'/
     *        1x,'Tolerances  ATOL : ',1PD9.1/
     *        1x,'     BTOL(phase) : ',1PD9.1/
     *        1x,'     BTOL(ampl.) : ',1PD9.1/
     *        1x,'Print-control NAG: ',i1/
     *        1x,'    Closure-phase: ',i1/
     *        1x,'  Solution-Report: ',i1/
     *        1x,'No-Fiddle-phase  : ',l1)

       write (iout,20) its_done, inform,
     *                 anorm, acond, rnorm, arnorm, xnorm
20     format(1x/
     *        1x,'Results:  From NAG solution'/
     *        1x,'---------------------------'/
     *        1x,'            Phase-solution    Amplitude-Solution'/
     *        1x,'Iterations: ',i14,4x,i18/
     *        1x,'How-ended : ',i14,4x,i18/
     *        1x,'ANORM     : ',1PD14.3,4X,1PD18.3/
     *        1x,'ACOND     : ',1PD14.3,4X,1PD18.3/
     *        1x,'RNORM     : ',1PD14.3,4X,1PD18.3/
     *        1x,'ARNORM    : ',1PD14.3,4X,1PD18.3/
     *        1x,'XNORM     : ',1PD14.3,4X,1PD18.3/1x)

        end
