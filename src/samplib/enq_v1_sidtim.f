C+ENQ_V1_SIDTIM

       subroutine enq_v1_sidtim ( stime1, stime2, s )
C
C     Returns the sidereal times from a sample file.
C
C     Returned:
C         Sidereal time (secs, mins, hours) at beginning of run
              INTEGER         STIME1(3)
C         Sidereal time (secs, mins, hours) at end of run
              INTEGER         STIME2(3)
C         Status
              INTEGER         S
C
C     This routine returns the sidereal times at the start and end
C     of the observing run.
C
C     Control Tables Version 1 support routine for ENQ_SID_TIME
C
C     DJT     3 May 1993
*-
      integer  i

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      do i = 1, 3
         stime1(i) = istim1(i)
         stime2(i) = istim2(i)
      enddo

      end



