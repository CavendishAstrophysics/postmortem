
C     *****************************************************************
C
C+lsf_enq_smooth
C
      SUBROUTINE lsf_enq_smooth( lsf_num,
     *                           sm_type,
     *                           sm_size,
     *                           sam_rate,
     *                           s                )

C
C     Returns the smoothing and sampling rates from the logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Smoothing type and smoothing count in samples
              integer             sm_type, sm_size
C         Sampling rate in samples
              integer             sam_rate
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
      include  '/mrao/post/include/lsf_definition.inc'
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
      sm_type = smooth_type
      sm_size = smooth_size
      sam_rate = samp_rate

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_SMOOTH' )
          return
      end
