C+SET_ION_CORR

       subroutine set_ion_corr ( lun,
     *                           corr_num,
     *                           type, key, lsf_key,
     *                           num_srcs, src_name, ra, dec,
     *                           param,
     *                           s               )
C
C     Sets ionospheric correction information for a given correction.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         Correction number
              integer         corr_num
C         Correction parameters
              integer         type, key, lsf_key
              integer         num_srcs
              character*(*)   src_name( num_srcs )
              real*8          ra( num_srcs), dec( num_srcs )
              integer         param(*)
C
C     Returned:
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Sets the supplied information into the ionospheric correction
C     redtape block.
C
C     Possible return status's are:
C         NOT_PHYSF       - File is not a physical sample file.
C         NO_IONCORR      - Correction not available.
C         TOO_MANYSRCS    - Too many source defined.
C
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/ion_definition.inc'

      integer     i, src_offset, total_srcs, source_diff

      if ( s .ne. 0 ) return

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

C     Do a few tests to begin with.
      if ((corr_num .le. 0) .or. (corr_num .gt. num_corrs) .or.
     *    (num_srcs .le. 0)                                     ) then
          s = NO_IONCORR
          return
      end if

C     Ensure space for the source information.
      total_srcs = 0
      do i = 1, num_corrs
          total_srcs = total_srcs + ion_srcs(i)
          if (i.eq.corr_num) src_offset = total_srcs
      enddo

      source_diff = (num_srcs-ion_srcs(corr_num))
      if ((total_srcs+source_diff).gt.max_ionsrcs) then
          s = TOO_MANYSRCS
          goto 999
      else if (corr_num .eq. num_corrs .or. source_diff .eq. 0) then
          continue
      else if (source_diff .lt. 0) then
C         Shuffle the sources down to fill up space
          do i = src_offset, total_srcs
              ion_source(i+source_diff) = ion_source(i)
          end do
      else
C         Shuffle the sources up to create space
          do i = total_srcs, src_offset, -1
              ion_source(i+source_diff) = ion_source(i)
          end do
      end if
      src_offset = src_offset - ion_srcs(corr_num)

C     Fill in information.
      ion_type(corr_num) = type
      ion_key(corr_num)  = key
      ion_lsf(corr_num)  = lsf_key
      ion_srcs(corr_num) = num_srcs
      do i = 1, num_srcs
          ion_source(src_offset+i) = src_name(i)
          ion_ra(src_offset+i)     = ra(i)
          ion_dec(src_offset+i)    = dec(i)
      enddo
      do i = 1, max_ionpars
          ion_param(i,corr_num) = param(i)
      enddo

C     Write correction redtape back to disc
      call write_ion_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine SET_ION_CORR' )

      end
