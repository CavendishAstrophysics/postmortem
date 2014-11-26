C+ENQ_V2_AESTAT

       subroutine enq_v2_aestat ( iae, option, test, s )
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
C     Control tables version 2 support routine for ENQ_AESTAT.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer  i

      if ( s .ne. 0 ) return

      test = .false.
      if (iae.ge.1 .and. iae.le.max_aes) then
        if (option.eq.0) then
          test = .true.
        elseif (option.eq.1 .or. option.eq.2) then
          do i = 1, naes
            if (iae.eq.iae_code(i)) test = .true.
          enddo
        elseif (option.eq.3) then
          test = aestatus(iae).eq.1.or.aestatus(iae).eq.3
        end if
      else
        s = ILL_AERIAL
      end if

      end
