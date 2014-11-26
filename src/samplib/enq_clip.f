C+ENQ_CLIP

       subroutine enq_clip ( lun, clip_level, s )
C
C     Returns the clip level (maximum amplitude) of the sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer         lun
C
C     Returned:
C         Maximum amplitude in external units (Jy)
              real*4          clip_level
C         Status
              integer         s
C
C     DJT     9 February 1995
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_clip( clip_level, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_clip( clip_level, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_CLIP' )

      end
