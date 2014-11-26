C+ENQ_V2_INTEG

       subroutine enq_v2_integ ( integ_time, samp_integ, s )
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
C     Control tables version 2 support routine for ENQ_INTEG.
C
C     Each recorded sample results from the integration of samp_count
C     correlator samples.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if ( s .ne. 0 ) return

      integ_time = integration*10
      samp_integ = integration/(micro_int*(hardware_int/1000))

      end



