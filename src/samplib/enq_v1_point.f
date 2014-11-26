



C+ENQ_V1_POINT

       subroutine enq_v1_point ( point_ra, point_dec, s )
C
C     Returns the nominal aerial pointing of a sample file.
C
C     Returned:
C         Phase center of aerials at epoch (I think).
              REAL*8          POINT_RA, POINT_DEC
C         Status
              INTEGER         S
C
C     For CT version 1.
C
C     PA      27 February 1989
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      point_ra  = raae
      point_dec = decae

      end
