C+CREATE_ION_CORR

      subroutine create_ion_corr ( lun,
     *                             first_samp,
     *                             last_samp,
     *                             corr_num,
     *                             s          )
C
C     Creates a new ionospheric correction within a sample file.
C
C     Given:
C         Logical unit number of physical sample file.
              integer     lun
C         First sample of range of correction.
              integer     first_samp
C         Last sample of range of correction.
              integer     last_samp
C
C     Returned:
C         Correction number within sample file.
              integer     corr_num
C         Status - must be zero on entry
              integer     s
C
C     Creates an entry for a new ionospheric correction, updating
C     the real-time common blocks appropriately to accommodate the
C     specified sample range.
C
C     Note that the parameters describing the new correction are not
C     added to the redtape by this routine;  use subsequent call to
C     SET_ION_CORR.
C
C     Possible error return status:
C         NO_IONCORR       - Correction not available.
C
C     DJT, NPR,   October 1987.
C-
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/ion_definition.inc'
C

      if ( s.ne.0 ) return

C     Read correction redtape
      call read_ion_ct ( lun, s )
      if ( s.ne.0 ) goto 999

C     Update correction redtape

      if ( num_corrs.lt.max_corrs ) then
          corr_num = num_corrs + 1
          ion_lsf(  corr_num ) = 0
          ion_type( corr_num ) = 0
          ion_key( corr_num )  = 0
          ion_first(corr_num ) = first_samp
          ion_last( corr_num ) = last_samp
          ion_srcs( corr_num ) = 0
          num_corrs = corr_num
      else
          s = NO_IONCORR
      endif

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine CREATE_ION_CORR' )

      end
