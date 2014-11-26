C+ENQ_V2_MJD0

       subroutine enq_v2_mjd0 ( mjd, s )
C
C     Returns the Modified Julian Date for the zero of sidereal time.
C
C     Returned
C         Modified Julian Date at the zero of Sidereal Time.
              real*8          mjd
C         Status value (must be zero on entry)
              integer         s
C
C     Control tables version 2 support routine for ENQ_MJD_ST0.
C
C     24-Aug-93 (DJT) Use MJDstart
C     28-Jun-96 (DJT) Patch for MJDstart close to midnight
C
*-
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      integer     start_st, start_ut

      if ( s .ne. 0 ) return

C     Use the run start time (expressed in the control tables as both
C     mJD, sidereal time and local time

      start_st = (istim1(3)*60 + istim1(2))*60 + istim1(1)
      start_ut = ((itim1(3)-iutim)*60 + itim1(2))*60 + itim1(1)
      mjd = int(MJDstart) +
     :              (start_ut - dble(start_st)/const_sut2sst)/86400.0D+0
C
C     Patch for cases in which MJDstart is very close to midnight
      if ( (MJDstart-mjd) .gt. 1.0D0) mjd = mjd+1.0D0

      end

