

C+ENQ_V1_MXPOINT

       subroutine enq_v1_mxpoint ( max_point, s )
C
C     Returns the maximum permitted pointing error
C
C     Returned:
C         Maximum pointing error (arcsecs)
              integer     max_point
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_MAX_POINT
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      max_point = 20

      end
