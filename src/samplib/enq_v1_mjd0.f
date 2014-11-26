

C+ENQ_V1_MJD0

       subroutine enq_v1_mjd0 ( mjd, s )
C
C     Returns the Modified Julian Date for the zero of sidereal time.
C
C     Returned
C         Modified Julian Date at the zero of Sidereal Time.
              real*8          mjd
C         Status value (must be zero on entry)
              integer         s
C
C     Control tables version 1 support routine for ENQ_MJD_ST0.
C
*-
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      integer     iepch(3), start_st, start_ut

      if ( s .ne. 0 ) return

C     Use the run start time (expressed in the control tables as both
C     sidereal time and
      call frddat( datobs, iepch )
      call sla_cldj( iepch(3), iepch(2), iepch(1), mjd, s )
      start_st = (istim1(3)*60 + istim1(2))*60 + istim1(1)
      start_ut = ((itim1(3)-iutim)*60 + itim1(2))*60 + itim1(1)
      mjd = mjd + (start_ut - dble(start_st)/const_sut2sst)/86400.0D+0

      end
