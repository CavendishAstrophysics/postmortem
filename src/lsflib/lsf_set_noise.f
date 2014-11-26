
C     *****************************************************************


C
C+lsf_set_noise
C
      SUBROUTINE lsf_set_noise( lsf_num,
     *                          mean, sigma,
     *                              s                              )

C     Defines the mean and sigma of the noise to add to the LSF model.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         noise mean and sigma (Jy)
              real*4              mean, sigma

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C     PJW 28/2/92 after lsf_set_bandpass
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Local variables

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

      if (sf_type .ne. 1) then
C         Not a physical sample file.
          call io_wrout('sample file of unexpected type')
C          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Set up runtime common blocks.

      mod_noise_mean = mean
      mod_noise_sigma = sigma

      if (s .ne. 0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_noise' )
          return
      end
