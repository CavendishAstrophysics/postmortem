C+WRITE_RT

      subroutine write_rt ( lun,
     *                      src_num,
     *                      samp_num,
     *                      ra, dec,
     *                      sid_time,
     *                      s           )
C
C     Writes redtape information for a single sample.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C         The number of the remove or calibration source to be read.
              integer        src_num
C         The number of the sample to be read.
              integer        samp_num
C
C     Returned:
C         Coordinates of phase centre.
              real*8         ra, dec
C         Sidereal time for sample (1/10ths second)
              integer        sid_time
C         Status variable - must be zero on entry otherwise error.
              integer        s
C
C     Writes the phase centre and sidereal time associated with a
C     given sample.
C
C     The details of the sample redtape can be found on the file
C     samp_rt.inc'.  The file must be opened via OPEN_SF, OPEN_RF or
C     OPEN_CF before this routine is called.
C
C     DJT,    October 1987
C-

C     Global includes -
C
      include '/mrao/post/include/samp_rt.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_buffer.inc'
      include '/mrao/include/iolib_errors.inc'

C     Local variable declarations -
C
C         Loop counter
              integer       i
C         Pointers to positions in file buffer
              integer       offset, rt_offset

      if ( s .ne. 0 ) return

      call read_buffer ( lun, src_num, samp_num, s )
      if ( s .eq. END_FILE) s = 0
      if ( s .ne. 0 ) goto 999

      num_vis_corr  = sf_num_vis
      samp_status   = 0
      samp_ra       = ra
      samp_dec      = dec
      samp_sid_time = sid_time
      samp_wt       = 10

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          buffer( rt_offset+i )= samp_rt( i )
   10 continue

      update_flag = update_flag + 1

      return

C     Error Handling -

 999  call smp_wrerr( s, 'in subroutine WRITE_RT' )

      end
