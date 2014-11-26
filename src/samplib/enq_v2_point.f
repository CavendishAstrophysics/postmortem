


C+ENQ_V2_POINT

       subroutine enq_v2_point ( point_ra, point_dec, s )
C
C     Returns the nominal aerial pointing of a sample file.
C
C     Returned:
C         Phase center of aerials at epoch.
              REAL*8          POINT_RA, POINT_DEC
C         Status
              INTEGER         S
C
C     For CT version 2.
C
C     The pointing centre is not a defined quantity for V2 control tables
C     This routine returns the phase centre at date except for the case of
C     a fixed pointing offset, in which case the 'true' pointing centre is
C     returned.
C
C     PA      27 February 1989
C     DJT     19 March 1992
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/include/constants.inc'

      if ( s .ne. 0 ) return

      if (offset.eq.4 .and. centre.eq.1) then
        point_ra  = radate - (60.d0*offset_W*const_sa2r)/cos(decdate)
        point_dec = decdate + (60.d0*offset_N*const_sa2r)
      else
        point_ra  = radate
        point_dec = decdate
      endif

      end
