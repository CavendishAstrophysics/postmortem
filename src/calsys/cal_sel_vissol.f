

C     *****************************************************************


C+cal_sel_vissol
C
      subroutine cal_sel_vissol( s )

C     Ask the user whether solution it be be visibility-based
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'

C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Buffer for current values of cal_record
              integer         buffer( cal_length )
C         Default string
              character*3     default

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      do 100, i = 1, cal_length
          buffer(i) = cal_record(i)
  100 continue

C
C     Main Code
C     ---------
C
      if (cal_type.eq.1) return
      if (cal_type.eq.2) then
        default = 'off'
      else
        default = 'on'
      end if
      if (io_onoff('Visibility-based-calibration :  ',default,s)) then
        cal_type = 3
      else
        cal_type = 2
      end if
      if (s .ne. 0) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call cal_wrerr( s, 'in subroutine cal_sel_vissol ' )
          end if
          do 9000, i = 1, cal_length
              cal_record(i) = buffer(i)
 9000     continue
          return
      end
