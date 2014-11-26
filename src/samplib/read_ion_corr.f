C+READ_ION_CORR

      subroutine read_ion_corr ( lun,
     *                           corr_num,
     *                           samp_num,
     *                           corr_buff,
     *                           s            )
C
C     Returns an ionospheric correction for a single sample.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer             lun
C         The number of the ionospheric correction.
              integer             corr_num
C         The number of the sample to be read.
              integer             samp_num
C
C     Returned:
C         Phase correction buffer
              real                corr_buff ( 4 )
C         Status variable - must be zero on entry otherwise error.
              integer             s
C
C     A low level routine for reading the ionospheric correction for a
C     given sample.  The phase of the correction is returned in units
C     of radians/wavelength.
C
C     If the correction status bit indicates that the correction is
C     not present for this sample then a status of NO_IONCORR is
C     returned, the correction buffer is set to zeroes and no error
C     logged.
C
C     Otherwise, if the correction is an old style correction, and if
C     both cos and sin of the correction are zero, then the correction
C     buffer is again cleared, a status of ILL_IONCORR is returned and
C     the error is logged.
C
C     NPR, DJT,   October 1987 (Adapted for new corrections)
C-

C     Global includes -
C
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/ion_definition.inc'
      include  '/mrao/post/include/samplib_errors.inc'

C     Local variable declarations -

C         Loop counter
              integer     i
C         Status word indicating which corrections are present:
C         - bit i set indicates correction with index i is present
              integer     ion_status
C         Pointers to positions in file buffer
              integer     offset, rt_offset, ion_offset, ptr
C         Weight and normalisation factor for the visibilities
              real        fact
C         Ionospheric correction cos and sine
              real        corr_cos, corr_sin, corr_rms, corr_angle

              intrinsic   btest

      if ( s .ne. 0 ) return

C     Read the correction redtape

      call read_ion_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      do i = 1, 4
          corr_buff(i) = 0.0
      end do

      if ( corr_num.lt.1 .or. corr_num.gt.num_corrs ) s = NO_IONCORR

C     Read sample into the file buffer if necessary
      call read_buffer( lun, 1, samp_num, s )
      if ( s .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      if ( samp_num .lt. ion_first(corr_num) .or.
     *     samp_num .gt. ion_last(corr_num) ) then
C         Correction is defined to be zero - so do nothing.

      else if (corr_num .le. num_old_corrs ) then
C         Old correction - read the sample redtape
          rt_offset = offset + start_rt - 1
          do  i = 1, length_rt
              samp_rt( i )= buffer( rt_offset+i )
          enddo

C         Check that correction is available for this sample
          if ( .not.btest(samp_status, 7+corr_num) ) then
              s = NO_IONCORR
          else
C             Extract the correction cos and sine from the sample, and
C             return the phase.
              ion_offset = offset + start_vis + num_vis_corr - 1
              fact = 1.0/amp_factor

              if ( data_type .eq. 1 ) then
                  ptr = 2*ion_offset + 4*corr_num - 3
                  corr_cos   = fact*float(ibuff(ptr))
                  corr_sin   = fact*float(ibuff(ptr+1))
                  corr_rms   = fact*float(ibuff(ptr+2)) * samp_wt
                  corr_angle = fact*float(ibuff(ptr+3)) * samp_wt
                  if ((corr_sin .ne. 0.0) .or. (corr_cos .ne. 0.0)) then
                      corr_buff(1) =
     *                        atan2(corr_sin,corr_cos) / ion_factor
                      corr_buff(3) =
     *                        sqrt(corr_cos*corr_cos+corr_sin*corr_sin)
                  else
                      s = ILL_IONCORR
                      goto 999
                  end if
              else
                  s = ILL_DATATYPE
                  goto 999
              end if
          end if
      else
C         New style correction, adjust correction number
          ion_offset = offset + start_ion
          ion_status = buffer(ion_offset)

C         Check that the correction is available for this sample
          if ( .not.btest(ion_status, ion_index(corr_num)-1) ) then
              s = NO_IONCORR
          else
              ptr = ion_offset + 4*ion_index(corr_num) - 3

              corr_buff(1) = rbuff(ptr)
              corr_buff(2) = rbuff(ptr+1)
              corr_buff(3) = rbuff(ptr+2)
              corr_buff(4) = rbuff(ptr+3)
          end if
      end if

      return

C     Error handling -

 999  call smp_wrerr( s, 'in subroutine READ_ION_CORR')

      end
