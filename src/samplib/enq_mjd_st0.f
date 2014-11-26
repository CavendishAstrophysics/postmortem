C+ENQ_MJD_ST0

       subroutine enq_mjd_st0 ( lun, mjd, s )
C
C     Returns the Modified Julian Date for the zero of sidereal time.
C
C     Given:
C         Sample file unit number.
              integer         lun
C
C     Returned:
C         Modified Julian Date at the zero of Sidereal Time.
              real*8          mjd
C         Status value (must be zero on entry)
              integer         s
C
C     This routine is provided so that routines can interface properly
C     with SLALIB routines. The modified julian date of a sample
C     with sidereal time ST (units 10ths of seconds) is:
C
C         MJD(ST) = MJD(0) + ST/(864000D+10*CONST_SUT2SST)
C
C     Where MJD(0) is the value returned by this routine.
C
C     NPR  16 February 1988
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_mjd0( mjd, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_mjd0( mjd, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_MJD_ST0' )

      end
