
C     *****************************************************************
C
C+lsf_sel_smooth
C
      SUBROUTINE lsf_sel_smooth ( lsf_num,
     *                            s                      )

C
C     Asks the user to select the lsf smoothing parameters.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present simply asks for smoothing size in samples and whether
C     sampling is at the same rate or every sample.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/iolib_functions.inc'

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
      lsf_key  = 1
      lsf_name = ' '

      call io_geti('Smooth size   (samples) : ', '1', smooth_size, s )
      smooth_size = max( 1, min( num_samp, smooth_size ) )

      if ( smooth_size .ne. 1 ) then
          smooth_type = 1
          samp_rate   = smooth_size
      else
C         No Smoothing
          smooth_type = 0
          smooth_size = 0
          samp_rate   = 1
      end if

      call io_geti('Sampling rate (samples) : ', '*', samp_rate, s )
      samp_rate = max( 1, min( num_samp, samp_rate ) )

C     Reset current sample and current buffer since they are now invalid
      curr_samp  = 0
      curr_buff  = 0
      start_buff = 0

      return

 1000 format( X, '*** Sampling must be between ', I2, ' and ', I4)

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .eq. usr_break ) then
              if ((smooth_size.lt.0).or.(smooth_size.ge.num_samp)) then
                  smooth_type = 0
                  smooth_size = 0
              end if
          else
              call lsf_wrerr( s, 'in subroutine LSF_SEL_SMOOTH' )
          end if
          return
      end
