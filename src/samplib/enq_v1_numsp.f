

C+ENQ_V1_NUMSP

       subroutine enq_v1_numsp ( numsp, s )
C
C     Returns the number of spacings from control tables.
C
C     Returned:
C         Number of spacings
              integer     numsp
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_NUMSP.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      numsp = nsp

      end
