

C+ENQ_V1_TSCOPE

       subroutine enq_v1_tscope ( tscope_name, tscope_code, s )
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
C     Control tables version 1 support routine for ENQ_TSCOPE.
C
C     The information is obtained from the ITSCOPE variable in the
C     current control tables. Supported values of ITSCOPE are:
C
C         ITSCOPE = 1, TSCOPE_NAME = 'T151',  TSCOPE_CODE = 1
C         ITSCOPE = 2, TSCOPE_NAME = '38MHZ', TSCOPE_CODE = 2
C         ITSCOPE = 3, TSCOPE_NAME = 'VLBI',  TSCOPE_CODE = 3
C
C     The observation frequency is checked to confirm this so this
C     routine can be used as a quick check for valid control tables.
C
C     NPR     29 June 1987
C
C     Patched because ITSCOPE apparently not set properly in 151 MHz
C     sample files. If ITSCOPE = 784 and the frequency is correct
C     then the telescope is T151
C
C     NPR     2 July 1987
C
C-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      if ((itscope .eq. 1) .or. (itscope .eq. 784)) then
C         151 MHz telescope
          if ((150.0d+6 .lt. frobs) .and. (frobs .lt. 152.0d+6)) then
              tscope_code = 1
              tscope_name = 'T151'
          else
              s = ILL_CONTTAB
          end if
      else if (itscope .eq. 2) then
C         38 MHz telescope
          if ((37.0d+6 .lt. frobs) .and. (frobs .lt. 39.0d+6)) then
              tscope_code = 2
              tscope_name = '38MHZ'
          else
              s = ILL_CONTTAB
          end if
      else if ( itscope .eq. 3 ) then
C         Cambridge VLBI telescope
          if ((80.0d+6 .lt. frobs) .and. (frobs .lt. 82.0d+6)) then
              tscope_code = 3
              tscope_name = 'VLBI'
          else
              s = ILL_CONTTAB
          end if
      else
          s = ILL_CONTTAB
      end if

      end
