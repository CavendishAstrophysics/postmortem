

C+ENQ_V2_NUMVIS

       subroutine enq_v2_numvis ( numvis, s )
C
C     Returns the number of visibilities per sample from control tables.
C
C     Returned:
C         Number of spacings
              integer     numvis
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_NUMVIS.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if ( s .ne. 0 ) return

      numvis = Nsp*Nsubb*Nchannel

      end
