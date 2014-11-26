
C     *****************************************************************
C
C+lsf_get_sid
C
      SUBROUTINE lsf_get_sid( lsf_num,
     *                        sid,
     *                        s                      )

C
C     Returns the sidereal time for the current LSF buffer.
C
C     Given:
C         Logical sample file number.
              integer             lsf_num

C     Returned:
C         Sidereal time expressed in 10ths of a second since sidereal
C         midnight on the day the observation started.
              integer             sid
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Returns the sidereal time of the current visibilities.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      sid = samp_sid

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_GET_SID' )
          return
      end
