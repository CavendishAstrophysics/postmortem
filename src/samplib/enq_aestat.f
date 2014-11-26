

C+ENQ_AESTAT

       subroutine enq_aestat ( lun, iae, option, test, s )
C
C     Tests the aerial status
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C         Aerial number
              integer     iae
C         Option
              integer     option
C                                   = 0    null test returns true always
C                                     1    is aerial in use?
C               (not implemented)     2    is aerial receiving?
C               (       "       )     3    is aerial tracking?
C
C     Returned:
C         result of the test
              logical     test
C         Status
              integer     s
C
C     PA 25/11/88
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_aestat( iae, option, test, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_aestat( iae, option, test, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_AESTAT' )

      end
