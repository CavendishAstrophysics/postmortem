
C+ENQ_TSCOPE

       subroutine enq_tscope ( lun, tscope_name, tscope_code, s )
C
C     Returns the telescope name and an integer code.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer         lun
C
C     Returned:
C         Telescope name.
              character*(*)   tscope_name
C         Telescope code.
              integer         tscope_code
C         Status
              integer         s
C
C     The information is obtained from the ITSCOPE variable in the
C     current control tables. Supported values of ITSCOPE are:
C
C         ITSCOPE = 1, TSCOPE_NAME = 'T151',  TSCOPE_CODE = 1
C         ITSCOPE = 2, TSCOPE_NAME = '38MHZ', TSCOPE_CODE = 2
C         ITSCOPE = 3, TSCOPE_NAME = 'VLBI',  TSCOPE_CODE = 3
C         ITSCOPE = 4, TSCOPE_NAME = 'RYLE',  TSCOPE_CODE = 4
C         ITSCOPE = 5, TSCOPE_NAME = 'RYLE',  TSCOPE_CODE = 5
C
C     The observation frequency is checked to confirm this so this
C     routine can be used as a quick check for valid control tables.
C
C     NPR     29 June 1987
C
C-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      character*(80)      str

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_tscope( tscope_name, tscope_code, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_tscope( tscope_name, tscope_code, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  write(str,'(a,i2,a)') ' tscope_code: ',
     *                      tscope_code , ' in subroutine ENQ_TSCOPE'
      call smp_wrerr( s, str )

      end
