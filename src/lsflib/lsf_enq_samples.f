
C     *****************************************************************
C
C+lsf_enq_samples
C
      SUBROUTINE lsf_enq_samples( lsf_num,
     *                            buff_num,
     *                            first_samp, last_samp,
     *                            s                    )

C
C     Returns the physical sample range applicable to the given buffer
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Buffer number
              integer             buff_num
C
C     Returned:
C         First and last physical sample for the specified buffer number
              integer             first_samp, last_samp
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
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         First valid sample in the buffer
              integer             valid_samp

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
      call sample_num( buff_num, valid_samp, first_samp, s )
      last_samp = min((first_samp + samp_rate - 1), num_samp )
      if ((s .ne. 0) .and. (s .ne. ILL_BUFFER)) goto 9999
      s = 0

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_SAMPLES' )
          return
      end
