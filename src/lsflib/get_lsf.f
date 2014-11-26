
C     *****************************************************************
C
C$(8) Internal LSFLIB routines.
C
C+get_lsf
C
      SUBROUTINE get_lsf( lsf_num, s )

C
C     Restores the specified logical sample file to the common blocks.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Saves the current lsf and reads the information for the specified
C     logical sample file into the current lsf common blocks. If the
C     specified lsf does not exist a status of ILL_LSF is returned.
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
C     Local variables, equivalences and commons

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( lsf_num .ne. curr_lsf_num ) then
          s = ILL_LSF
          goto 9999
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine GET_LSF' )
          return
      end
