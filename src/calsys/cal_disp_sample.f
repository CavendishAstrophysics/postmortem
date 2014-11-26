

C     *****************************************************************
C
C+cal_disp_sample
C
      SUBROUTINE cal_disp_sample( plot_device, s )

C     Displays samples for the current calibration model.
C
C     Given:
C         The PGPLOT plot device.
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'

C
C     Variables, equivalences and commons
C         Temporary status
              integer         temp_s
C         Logical sample file number
              integer         lsf_num

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C     Main Code
C     ---------
C
      call cal_open_lsf( 'READ', lsf_num, s )
      call display_sample( lsf_num, .false., plot_device, s )

      temp_s = 0
      call lsf_close( lsf_num, temp_s )
      if ( s .ne. 0 ) goto 9999

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if ( s .ne. USR_BREAK) then
              call cal_wrerr( s, 'in subroutine cal_disp_sample ' )
          end if
          return
      end
