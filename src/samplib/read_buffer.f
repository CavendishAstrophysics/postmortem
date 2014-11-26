
C+READ_BUFFER

      subroutine read_buffer ( lun, src_num, samp_num, s )
C
C     Reads file buffer containing a given sample.
C
C     Given:
C         The logical unit number of the sample file.
              INTEGER        LUN
C         The number of the remove or calibration source to be read.
              INTEGER        SRC_NUM
C         The number of the sample to be read.
              INTEGER        SAMP_NUM
C
C     Returned:
C         Status variable - must be zero on entry otherwise error.
              INTEGER        S
C
C     Checks that the file buffer contains the requested sample and
C     reads a new buffer from disc if necessary.  The current buffer
C     will be written to disc if it has been updated.
C
C-

C     Global includes -
C
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_buffer.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/include/iolib_errors.inc'

C     Local variable declarations -
C
C         Byte number and block number of the current sample.
              integer     word_num, block_num
C         Number of words to be read.
              integer     word_count
              integer     nword
C
      if ( s .ne. 0 ) return

C     Check to see if the file information is in the file control block
C     - if not retrieve it.

      if ((lun .ne. sf_lun) .or. (src_num .ne. sf_src_num)) then
          call get_sf_pack( lun, src_num, s )
          if ( s .ne. 0 ) goto 999
      end if

C     Check sample number in range

      if ( samp_num .lt. 1 .or. samp_num .gt. num_samp ) then
          s = ILL_SAMPLE
          goto 999
      end if

C     Check whether sample is already in the file buffer, otherwise read
C     a new buffer starting at the required sample.  The current buffer
C     will be flushed to disc if it has been updated.

      if ( samp_num .ge. first_samp .and.
     *     samp_num .le. last_samp  ) then
          curr_samp = samp_num
      else
          if ( update_flag .gt. 0 ) then
c	write(*,*)' write buffer',lun
              call write_buffer ( lun, src_num, s )
              if ( s .ne. 0 ) goto 999
          end if

C         Find the word and block where the sample starts.
          word_num   = sf_first_samp_ptr + (samp_num-1)*samp_len
          block_num  = int( word_num / block_size) + 1

C         Find the offset of the first sample in the buffer
          data_offset= mod( word_num, block_size )

C         Find out how many words to transfer, and ensure it is a multiple
C         of the block size.
          word_count= min((data_offset+(num_samp-samp_num+1)*samp_len),
     *                     buffer_len                                  )
          word_count= (int((word_count-1)/block_size)+1)*block_size
c	write(*,*)' read buffer',lun,samp_num,block_num,
c    *   data_offset,word_count,samp_len,buffer_len

          nword = word_count
          call io_rdfile( lun,
     *                    block_num,
     *                    buffer(buffer_ptr),
     *                    nword,
     *                    s )
          if ( s .eq. -1) then
             s = END_FILE
          else if ( s .ne. 0 ) then
             goto 999
          end if

          first_samp = samp_num
          curr_samp  = samp_num
          last_samp  = first_samp-1 + (word_count-data_offset)/samp_len
c	write(*,*)' rdfile',lun,first_samp,curr_samp,last_samp,word_count
c         call rfile_log( lun, first_samp, curr_samp, last_samp )

          update_flag = 0

      end if

      return

C     Error Handling -

 999  call smp_wrerr( s, 'in subroutine READ_BUFFER')

      end
