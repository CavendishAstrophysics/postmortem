C+lsf_set_integt
C
      SUBROUTINE lsf_set_integt( lsf_num,
     *                              integ_time,
     *                              s                              )

C     Defines the telescope integration time(sampling)for the LSF model.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         integration-time in seconds
              real*4              integ_time

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C
C     PJW 8/91 after lsf_set_bandpass
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

      if ( integ_time .eq. 0.0 ) then
          mod_integ_flg = .false.
          mod_integ_time = 0.0
      else
          mod_integ_flg = .true.
          mod_integ_time = integ_time
      endif

C      write(1,*) 'model integration time set at:',mod_integ_time

      if (s .ne. 0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_integt' )
          return
      end
