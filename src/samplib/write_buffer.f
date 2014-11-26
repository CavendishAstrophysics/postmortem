

C+WRITE_BUFFER

      subroutine write_buffer ( lun, src_num, s )
C
C     Writes current file buffer to disc.
C
C     Given:
C         The logical unit number of the sample file.
              INTEGER        LUN
C         The number of the remove or calibration source.
              INTEGER        SRC_NUM
C
C     Returned:
C         Status variable - must be zero on entry otherwise error.
              INTEGER        S
C
C     Checks whether the file buffer has been updated, and writes the
C     buffer to disc if necessary.
C
C-

C     Global includes -
C
      include  '/mrao/post/include/sf_pack.inc'
      include  '/mrao/post/include/sf_buffer.inc'
      include  '/mrao/post/include/samplib_errors.inc'

C     Local variable declarations -
C
C         Byte number and block number of first sample in the buffer.
              integer     word_num, block_num
C         Number of words to be written.
              integer     word_count
C
      if ( s .ne. 0 ) return

C     Check to see if the file information is in the file control block
C     - if not retrieve it.

      if ((lun .ne. sf_lun) .or. (src_num .ne. sf_src_num)) then
          call get_sf_pack( lun, src_num, s )
          if ( s .ne. 0 ) goto 999
      end if

C     Check whether the buffer has been updated since it was initialised
C     or read from disc, and so needs to be written.

      if ( update_flag .gt. 0 ) then
          word_num   = sf_first_samp_ptr + (first_samp-1)*samp_len
          block_num  = int( word_num / block_size) + 1

C         Find out how many words to transfer, and ensure its a multiple
C         of the block size.
          word_count= min((data_offset +
     *                     (last_samp-first_samp+1)*samp_len),
     *                     buffer_len                                  )
          word_count= (int((word_count-1)/block_size)+1)*block_size

          call io_wrfile( lun,
     *                    block_num, 
     *                    buffer(buffer_ptr),
     *                    word_count,
     *                    s )
          if ( s .ne. 0 ) goto 999

C         call wfile_log( lun, first_samp, curr_samp, last_samp )
          update_flag = 0

      end if

      return

C     Error Handling -

 999  call smp_wrerr( s, 'in subroutine WRITE_BUFFER')

      end
