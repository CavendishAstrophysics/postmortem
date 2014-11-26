C+ENQ_V2_CLIP

       subroutine enq_v2_clip ( clip, s )
C
C     Returns the clip level (maximum amplitude) from control tables.
C
C     Returned:
C         Maximum amplitude in external units
              real*4          clip
C         Status
              integer         s
C
C     Control tables version 2 support routine for ENQ_UNITS.
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if ( s .ne. 0 ) return

      clip = clip_level

      end




