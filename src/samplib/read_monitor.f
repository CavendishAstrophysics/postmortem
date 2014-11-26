

C+READ_MONITOR

      subroutine read_monitor ( lun,
     *                          samp_num,
     *                          mon_length,
     *                          mon_block,
     *                          s           )
C
C     Returns the monitor block for a single sample.
C
C     Given:
C         The logical unit number of the sample file.
              integer             lun
C         The number of the sample to be read.
              integer             samp_num
C         The length of the monitor block.
              integer             mon_length
C
C     Returned:
C         The monitor block.
              integer*2           mon_block ( mon_length )
C         Status variable - must be zero on entry otherwise error.
              integer             s
C
C     A low level routine for reading the values of parameters monitored
C     during the observation.  The detailed contents of the monitor block
C     are described in the include file (POST)MON-BLOCK.inc'.  Readings of
C     AGC values, PC settings, aerial pointing and correlator error counts
C     are recorded with each sample, together with a count which is incremented
C     during the run whenever the monitored information is updated.
C
C     DJT,    June 1987
C-

C     Global includes -
C
      include  '/mrao/post/include/samp_rt.inc'
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/samplib_errors.inc'

C     Local variable declarations -

C         Loop counter
              integer     i
C         Pointers to positions in file buffer
              integer     offset, rt_offset, mon_offset

C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     Read the sample into the file buffer if necessary
C
      call read_buffer ( lun, 1, samp_num, s )
      if ( s .ne. 0 ) goto 999

      if (length_mon .eq. 0) then
          s = NOT_PHYSF
          goto 999
      end if

      offset = buffer_ptr + data_offset +
     *         (curr_samp-first_samp)*samp_len - 1

C     Read the sample redtape
C
      rt_offset = offset + start_rt - 1
      do 10 i = 1, length_rt
          samp_rt( i )= buffer( rt_offset+i )
   10 continue

C     Read the monitor block
C
      mon_offset = offset + start_mon - 1
      mon_offset = 2*mon_offset

      if ( data_type .eq. 1 ) then
          do i = 1, mon_length
              mon_block(i) = ibuff(mon_offset+i)
          enddo
      else
          s = ILL_DATATYPE
          goto 999
      end if

      return

C     Error handling -
C

 999  call smp_wrerr( s, 'in subroutine READ_MONITOR')

      end
