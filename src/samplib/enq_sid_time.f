C+ENQ_SID_TIME

       subroutine enq_sid_time ( lun, stime1, stime2, s )
C
C     Returns the sidereal times from a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              INTEGER         LUN
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
C     DJT     3 May 1993
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_sidtim( stime1, stime2, s )
      else if (ct_vers .eq. 2) then
         call enq_v2_sidtim( stime1, stime2, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_SID_TIME' )

      end


