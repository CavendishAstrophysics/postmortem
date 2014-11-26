C+ENQ_ION_CORR

       subroutine enq_ion_corr ( lun,
     *                           corr_num,
     *                           type, key, lsf_key,
     *                           first_samp, last_samp,
     *                           num_srcs, src_name, ra, dec,
     *                           param,
     *                           s               )
C
C     Returns ionospheric correction information for a given correction.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer         lun
C         Correction number
              integer         corr_num
C
C     Returned:
C         Correction parameters
              integer         type, key, lsf_key
              integer         first_samp, last_samp, num_srcs
              character*(*)   src_name(*)
              real*8          ra(*), dec(*)
              integer         param(*)
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Returns information from the ionospheric correction redtape
C     block.
C
C     Possible return status's are:
C         NOT_PHYSF        - File is not a sample file.
C         NO_IONCORR       - No such correction.
C
C-

C     Global includes -
C
      include '/mrao/post/include/ion_definition.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer     i, src_offset

      if ( s .ne. 0 ) return

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

      if ( corr_num .gt. 0 .and. corr_num .le. num_corrs ) then
          type        = ion_type(corr_num)
          key         = ion_key(corr_num)
          lsf_key     = ion_lsf(corr_num)
          first_samp  = ion_first(corr_num)
          last_samp   = ion_last(corr_num)
          num_srcs    = ion_srcs(corr_num)

          src_offset  = 0
          do i = 1, corr_num-1
              src_offset = src_offset + ion_srcs(i)
          enddo
          do i = 1, num_srcs
              src_name(i) = ion_source(src_offset+i)
              ra(i)       = ion_ra(src_offset+i)
              dec(i)      = ion_dec(src_offset+i)
          enddo

          do i = 1, max_ionpars
              param(i) = ion_param(i,corr_num)
          enddo
      else
          s = NO_IONCORR
      end if

      if ( s.ne.0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_ION_CORR' )

      end
