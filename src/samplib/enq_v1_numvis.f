

C+ENQ_V1_NUMVIS

       subroutine enq_v1_numvis ( numvis, s )
C
C     Returns the number of visibilities per sample from control tables.
C
C     Returned:
C         Number of visibilities
              integer     numvis
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_NUMVIS.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      numvis = nsp

      end
