

C     *****************************************************************
C
C+cal_sel_refant
C
      SUBROUTINE cal_sel_refant( psf_name, s )

C     Asks the user to select the reference antenna for the calibration
C
C     Given:
C         Current physical sample file name
              character*64        psf_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/cal_solution.inc'

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Main Code
C     ---------

10    continue
      call io_geti( 'Reference Antenna : ', '*', cal_refant, s )
      if (cal_refant.gt.max_RT_aes) then
        call io_wrout('*** Illegal reference antenna number')
        cal_refant = 5
        goto 10
      end if
      current_refant = cal_refant
      if ( s .ne. 0 ) goto 9999
      return


C     Error Handling
C     --------------

 9999 continue
          if ( s .ne. usr_break ) then
              call cal_wrerr( s, 'in subroutine cal_sel_refant ' )
          end if
          return
      end
