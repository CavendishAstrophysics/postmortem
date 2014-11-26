

C+ENQ_V1_POLN

       subroutine enq_v1_poln ( poln, s )
C
C     Returns the polarization code for the sample file
C
C     Returned:
C         Polarization code
               integer     poln
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_POLN
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      poln = -1

      end
