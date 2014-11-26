
C     *****************************************************************
C
C+lsf_enq_numbuff
C
      SUBROUTINE lsf_enq_numbuff( lsf_num,
     *                            num_buff,
     *                            s                    )

C
C     Returns the number of buffers in a logical sample file.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Number of last buffer in the logical sample file
              integer*4           num_buff
C         Status variable - must be zero on entry - otherwise error
              integer             s
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
C         Start of the last buffer
              integer             start_last_buff

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
C     Get last buffer number.
      call buffer_num( num_samp, num_buff, start_last_buff, s )
      if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999
      s = 0

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_NUMBUFF' )
          return
      end
