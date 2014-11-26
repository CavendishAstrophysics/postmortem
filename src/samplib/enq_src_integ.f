

C+ENQ_SRC_INTEG

       subroutine enq_src_integ ( lun, src_num, integration, s )
C
C     Returns the nominal integration of a source in a sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the remove or calibration source to be read.
              integer        src_num
C
C     Returned:
C         The integration time in 10ths of sidereal seconds.
              integer        integration
C         Status variable - must be zero on entry otherwise error.
              integer        s
C
C     N.B. The source must be opened before this routine is called.
C
C-
C     Global includes -
C
      include '/mrao/post/include/sf_pack.inc'

      if ( s .ne. 0 ) return

C     Check to see if the file information is in the file control block
C     - if not retrieve it.

      if ((lun .ne. sf_lun) .or. (src_num .ne. sf_src_num)) then
          call get_sf_pack( lun, src_num, s )
          if ( s .ne. 0 ) goto 999
      end if

      integration = (stop_time-start_time )/num_samp

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_SRC_INTEG' )

      end
