


C$(2)  Routines for reading from sample files.

C+READ_RT

      subroutine read_rt ( lun,
     *                     src_num,
     *                     samp_num,
     *                     ra, dec,
     *                     sid_time,
     *                     s           )
C
C     Returns redtape information for a single sample.
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
C     Returns the phase centre and sidereal time associated with a
C     given sample. The sidereal time is adjusted so that it is
C     returned as 10ths of a second since sidereal midnight on the
C     day the run commenced. This makes it a monotonically increasing
C     variable
C
C     The details of the sample redtape can be found on the file
C     samp_rt.inc'.  The file must be opened via OPEN_SF before this
C     routine is called.
C
C     NPR, DJT,   May 1987.
C-

C     Global includes -
C
      include '/mrao/post/include/samp_rt.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_buffer.inc'

C     Local variable declarations -
C
C         Loop counter
              integer         i
C         Pointers to positions in file buffer
              integer         offset, rt_offset
C         Estimated value of sidereal time
              integer         est_time

      if ( s .ne. 0 ) return

      call read_buffer ( lun, src_num, samp_num, s )
      if ( s .ne. 0 ) goto 999

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          samp_rt( i )= buffer( rt_offset+i )
   10 continue

      ra       = samp_ra
      dec      = samp_dec
      sid_time = samp_sid_time

C     Correct the sidereal time, if necessary.
      est_time = start_time+
     *           (stop_time-start_time)*(real(samp_num)-0.5)/num_samp
  20  if ( sid_time .lt. (est_time-432000)) then
         sid_time = sid_time + 864000
         goto 20
      end if

      return

 999  call smp_wrerr( s, 'in subroutine READ_RT' )

      end
