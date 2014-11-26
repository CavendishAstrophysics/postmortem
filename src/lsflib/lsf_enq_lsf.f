
C     *****************************************************************
C
C+lsf_enq_lsf
C
      SUBROUTINE lsf_enq_lsf( lsf_num,
     *                        file_name,
     *                        key,
     *                        s                    )

C
C     Returns the logical sample file name.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Full physical sample file name of the lsf.
              character*(*)       file_name
C         Logical sample file key.
              integer             key
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     If the lsf_num is zero the the values for the current lsf are
C     returned.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num .and. lsf_num .ne. 0 ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      file_name = sf_name
      key       = lsf_key

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_LSF' )
          return
      end
