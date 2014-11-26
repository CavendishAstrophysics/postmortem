
C     *****************************************************************
C
C+lsf_enq_numsp
C
      SUBROUTINE lsf_enq_numsp( lsf_num,
     *                          num_sp,
     *                          s                    )

C
C     Returns the number of spacings in the logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Number of spacings in the logical sample file.
              integer             num_sp
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
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

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      num_sp = sp_list_len

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_NUMSP' )
          return
      end
