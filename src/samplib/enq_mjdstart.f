C+ENQ_MJDstart

       subroutine enq_mjdstart (mjd, s)
C
C     Returns the Modified Julian Date at the start of RT obs
C
C     Returned
C         Modified Julian Date at the start of the observation.
              real*8          mjd
C         Status value (must be zero on entry)
              integer         s
C
C
*       30 Jan 2002 GP
*-
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'


      if (s .ne. 0) return

      mjd = MJDstart

      end

