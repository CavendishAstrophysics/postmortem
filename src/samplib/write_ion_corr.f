C+WRITE_ION_CORR

      subroutine write_ion_corr ( lun,
     *                            corr_num,
     *                            samp_num,
     *                            corr_buff,
     *                            s            )
C
C     Writes an ionospheric correction for a single sample.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer             lun
C         The number of the ionospheric correction.
              integer             corr_num
C         The number of the sample to be written.
              integer             samp_num
C         Phase correction buffer
              real                corr_buff ( 4 )
C
C     Returned:
C         Status variable - must be zero on entry otherwise error.
              integer             s
C
C     A low level routine for writing the ionospheric correction for
C     a given sample.
C
C     DJT, NPR,   October-November 1987.
C-

C     Global includes -
C
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/ion_definition.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/include/iolib_errors.inc'

C     Local variable declarations -

C         Pointers to positions in file buffer
              integer     offset, ion_offset, ptr
C         Intrinsic function
              intrinsic   ibset


      if ( s .ne. 0 ) return

C     Read the correction redtape

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

C     Check that the correction is defined for this sample
      if ( corr_num .le. num_old_corrs        .or.
     *     corr_num .gt. num_corrs            .or.
     *     samp_num .lt. ion_first(corr_num)  .or.
     *     samp_num .gt. ion_last(corr_num)          ) then
          s = NO_IONCORR
          goto 999
      endif

C     Read sample into file buffer if necessary
      call read_buffer( lun, 1, samp_num, s )
      if ( s .eq. END_FILE ) s = 0
      if ( s .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      ion_offset = offset + start_ion
      buffer(ion_offset) = 
     *         ibset(buffer(ion_offset),ion_index(corr_num)-1)

      ptr = ion_offset + 4*ion_index(corr_num) - 3
      rbuff(ptr)   = corr_buff(1)
      rbuff(ptr+1) = corr_buff(2)
      rbuff(ptr+2) = corr_buff(3)
      rbuff(ptr+3) = corr_buff(4)

      update_flag = update_flag + 1

      return

C     Error handling -

 999  call smp_wrerr( s, 'in subroutine WRITE_ION_CORR')

      end
