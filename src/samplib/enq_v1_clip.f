C+ENQ_V1_CLIP

       subroutine enq_v1_clip ( clip, s )
C
C     Returns the clip level (maximum amplitude) from control tables.
C
C     Returned:
C         Maximum amplitude in external units (Jy).
              real*4          clip
C         Status
              integer         s
C
C     Control tables version 1 support routine for ENQ_CLIP.
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if ( s .ne. 0 ) return

      clip = achop*ampscl

      end




