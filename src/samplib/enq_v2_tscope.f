

C+ENQ_V2_TSCOPE

       subroutine enq_v2_tscope ( tscope_name, tscope_code, s )
C
C     Returns the telescope name from control tables.
C
C     Returned:
C         Telescope name.
              character*(*)   tscope_name
C         Telescope code.
              integer         tscope_code
C         Status
              integer         s
C
C     Control tables version 2 support routine for ENQ_TSCOPE.
C
C     The information is obtained from the ITSCOPE variable in the
C     current control tables. Supported values of ITSCOPE are:
C
C         ITSCOPE = 4, TSCOPE_NAME = 'RYLE',  TSCOPE_CODE = 4
C         ITSCOPE = 5, TSCOPE_NAME = 'RYLE',  TSCOPE_CODE = 5
C
C     The observation frequency is checked to confirm this so this
C     routine can be used as a quick check for valid control tables.
C
C-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      tscope_code = itscope
      tscope_name = 'RYLE'

      end
