C+ENQ_INTEG

       subroutine enq_integ ( lun, integ_time, samp_integ, s )
C
C     Returns the integration time from the control tables.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C
C     Returned:
C         The integration time in 10ths of sidereal seconds.
              integer        integ_time
C         The correlator sample integration count.
              integer        samp_integ
C         Status variable - must be zero on entry otherwise error.
              integer        s
C
C     Each recorded sample results from the integration of samp_count
C     correlator samples.  The correlator integration time is therefore
C     integ_time/samp_integ.
C
C-
C     Global includes -
C
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_integ( integ_time, samp_integ, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_integ( integ_time, samp_integ, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_INTEG' )

      end


