
C     *****************************************************************
C

C+lsf_set_bandpass
C
      SUBROUTINE lsf_set_bandpass( lsf_num,
     *                             bandpass_type, bandpass_width,
     *                             s                              )

C     Defines the telescope bandpass filter type for the LSF model.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Bandpass filter type (1=boxcar,2=gauss), and FWHM in Hz or MHz
              integer             bandpass_type
              real                bandpass_width

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     NPR     6 April 1988.
C     PJW 10/91 Mods to deal with input in Hz or MHz
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
C         Telescope frequency
              real*8          freq
C         bandwidth in hz
              real*4          band_hertz

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

C     Sort out Hz or MHz input
      if ( bandpass_width .le. 1000. ) then
          band_hertz = bandpass_width*1e6
      else
          band_hertz = bandpass_width
      endif

      call enq_freq( sf_lun, freq, s )
      mod_band_type  = bandpass_type
      mod_band_width = band_hertz/freq

C      write(1,*) 'fractional bandwidth set to:',mod_band_width

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_BANDPASS' )
          return
      end
