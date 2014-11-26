


C+ENQ_POINT

       subroutine enq_point ( lun, point_ra, point_dec, s )
C
C     Returns the nominal aerial pointing of a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              INTEGER         LUN
C
C     Returned:
C         Phase center of aerials at epoch.
              REAL*8          POINT_RA, POINT_DEC
C         Status
              INTEGER         S
C
C     NPR     14 July 1987
C     PA      27 February 1989
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_point( point_ra, point_dec, s)
      else if (ct_vers .eq. 2) then
         call enq_v2_point( point_ra, point_dec, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_POINT' )

      end
