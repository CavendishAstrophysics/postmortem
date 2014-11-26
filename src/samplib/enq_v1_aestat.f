

C+ENQ_V1_AESTAT

       subroutine enq_v1_aestat ( iae, option, test, s )
C
C     Tests the aerial status
C
C     Given:
C         Aerial number
              integer     iae
C         Option code
              integer     option
C
C     Returned:
C         Results of the test
              logical     test
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_AESTAT.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/samplib_errors.inc'

      intrinsic  btest

      if ( s .ne. 0 ) return

      if (iae.ge.1 .and. iae.le.max_aes) then
        if (option.eq.0 .or. option.gt.1) then
          test = .true.
        else
          test = btest(iaestat(iae),13)
        end if
      else
        s = ill_aerial
      end if

      end
