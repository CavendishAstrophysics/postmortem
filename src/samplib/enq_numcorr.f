C+ENQ_NUMCORR

       subroutine enq_numcorr ( lun, num_corr, s )
C
C     Returns the number of ionospheric corrections in a sample file.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer         lun
C
C     Returned:
C         Number of corrections in the sample file.
              integer         num_corr
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Returns information from the ionospheric correction redtape
C     block.
C
C     Possible return status's are:
C         NOT_PHYSF        - File is not a sample file.
C
C-

C     Global includes -
C
      include '/mrao/post/include/ion_definition.inc'

      if ( s .ne. 0 ) return

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

      num_corr = num_corrs
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_NUMCORR' )

      end
