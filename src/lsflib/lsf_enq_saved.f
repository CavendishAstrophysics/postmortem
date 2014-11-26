C
C+lsf_enq_saved
C
      SUBROUTINE lsf_enq_saved(   lsf_num,
     *                            saved,
     *                            s                    )

C
C     Returns if the current lsf is saved or not.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Saved flag - set if the lsf is saved.
              logical             saved
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine can be used to find whether the current lsf is
C     saved (via the 'saved' returned parameter), but also to find
C     if there is any lsf's saved at all (ie if a :LSF file exists).
C
C     Returned status's:
C         0           -   Success and a :LSF file exists
C         NO_LSFSAVED -   No :LSF file (no error is logged)
C         Other       -   Unexpected error
C
C     NPR     14 October 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsflib_errors.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Logical sample file :LSF file name
              character*80    file_name

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
      call enq_namfil( sf_lun, 'LSF', file_name, s )
      if (s .eq. 0) then
          saved = (lsf_key .ne. 1)
      else if (s .eq. NO_FILE) then
          s = NO_LSFSAVED
      else
          goto 9999
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_SAVED' )
          return
      end
