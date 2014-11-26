

C+ENQ_MAX_POINT

       subroutine enq_max_point ( lun, max_point, s )
C
C     Returns maximum permitted pointing error
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         maximum permitted pointing error (arcsecs)
              integer     max_point
C         Status
              integer     s
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .eq. 1) then
         call enq_v1_mxpoint( max_point, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_mxpoint( max_point, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_MAX_POINT' )

      end
