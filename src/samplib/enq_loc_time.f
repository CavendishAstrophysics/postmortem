C+ENQ_LOC_TIME

       subroutine enq_loc_time ( lun, date1, time1, date2, time2, s )
C
C     Returns the local dates and times from a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              INTEGER         LUN
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
C     DJT     3 May 1993
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_loctim( date1, time1, date2, time2, s )
      else if (ct_vers .eq. 2) then
         call enq_v2_loctim( date1, time1, date2, time2, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_LOC_TIME' )

      end


