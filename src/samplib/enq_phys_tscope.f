

C+ENQ_PHYS_TSCOPE

       subroutine enq_phys_tscope( lun, tscope_code, s)
C
C     Returns the physical telescope type as an integer code
C
C     Given
C         Sample file Fortran logical unit number.
              integer         lun
C
C     Returned:
C         Physical Telescope code.
              integer         tscope_code
C         Status
              integer         s
C
C     The information is obtained from the ITSCOPE variable in the
C     current control tables which is found by calling enq_tscope.
C
C         ITSCOPE = 1,  CLFST 151MHz    TSCOPE_CODE = 1
C         ITSCOPE = 2,  CLFST 38MHz     TSCOPE_CODE = 1
C         ITSCOPE = 3,  VLBI 81.5MHz    TSCOPE_CODE = 3
C         ITSCOPE = 4,  RYLE 5000MHz    TSCOPE_CODE = 2
C         ITSCOPE = 5,       15400MHz   TSCOPE_CODE = 2
C
C     The observation frequency is checked to confirm this so this
C     routine can be used as a quick check for valid control tables.
C
C     [PA, 4/12/88]
C     [DJT, 29/7/92]
C
C-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/phys_tscopes.inc'
      include '/mrao/post/include/samplib_errors.inc'

      character*20  tscope_name
      integer       icode

      if ( s .ne. 0 ) return

      call enq_tscope( lun, tscope_name, icode, s)
      if (s.eq.0) then
        if (icode.le.2) then
          tscope_code = clfst
        else if (icode.eq.3) then
          tscope_code = vlbi_81p5MHz
        else if (icode.eq.4 .or. icode.eq.5) then
          tscope_code = ryle 
        end if
      else
        call smp_wrerr( s, 'in routine ENQ_PHYS_TSCOPE' )
      end if

      end
