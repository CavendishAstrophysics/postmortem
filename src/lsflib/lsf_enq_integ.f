
C     *****************************************************************
C
C+lsf_enq_integ
C
      SUBROUTINE lsf_enq_integ(   lsf_num,
     *                            integ_time,
     *                            s                    )

C
C     Returns the effective integration time of a logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Integration time ( in 10ths of sidereal seconds )
              integer             integ_time
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The integration time is basically the average sidereal time
C     between successive lsf buffers
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
      call enq_src_integ( sf_lun, src_num, integ_time, s )
      integ_time = integ_time*samp_rate

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_INTEG' )
          return
      end
