C+ENQ_V1_LOCTIM

       subroutine enq_v1_loctim ( date1, time1, date2, time2, s )
C
C     Returns the local dates and times from a sample file.
C
C     Returned:
C         Date (day, month, year) at beginning of run
              INTEGER         DATE1(3)
C         Local time (secs, mins, hours) at beginning of run
              INTEGER         TIME1(3)
C         Date (day, month, year) at end of run
              INTEGER         DATE2(3)
C         Local time (secs, mins, hours) at end of run
              INTEGER         TIME2(3)
C         Status
              INTEGER         S
C
C     This routine returns the dates and local times at the start and
C     end of the observing run.
C
C     Control Tables Version 1 support routine for ENQ_LOCAL_TIME
C
C     DJT     3 May 1993
*-

      integer  i

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      do i = 1, 3
         date1(i) = idat1(i)
         time1(i) = itim1(i)
         date2(i) = idat2(i)
         time2(i) = itim2(i)
      enddo

      end



