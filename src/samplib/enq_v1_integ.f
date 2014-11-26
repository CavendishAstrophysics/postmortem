C+ENQ_V1_INTEG

       subroutine enq_v1_integ ( integ_time, samp_integ, s )
C
C     Returns the integration time from control tables.
C
C     Returned:
C         Sample integration time in 1/10 sidereal sec
              integer     integ_time
C         Sample integration count
              integer     samp_integ
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_INTEG.
C
C     Each recorded sample results from the integration of samp_count
C     correlator samples.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'

      if ( s .ne. 0 ) return

      if ( abs(ismtyp) .eq. 1 ) then
         integ_time = 2*intsam*(itab(1)+itab(2))/1000
         samp_integ = intsam
      else
         integ_time = 2*intsam*isamps
         samp_integ = intsam
      endif

      end



