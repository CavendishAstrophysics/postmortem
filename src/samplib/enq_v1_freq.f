

C+ENQ_V1_FREQ

       subroutine enq_v1_freq ( freq, s )
C
C     Returns the observing frequency from control tables.
C
C     Returned:
C         Observing frequency (Hz).
               real*8      freq
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_FREQ.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      freq = frobs

      end
