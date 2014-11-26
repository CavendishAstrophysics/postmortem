

C+CLOSE_SOURCE

      subroutine close_source ( lun, src_num, s )
C
C     Closes a source for given sample file.
C
C     Given:
C         Logical unit number of sample file
              integer         lun
C         Source number to close up.
              integer         src_num

C     Returned:
C         Status - must be zero on entry.
              integer         s
C
C     This routine essentially unwinds everything done by OPEN_SOURCE.
C     It does not actually close the file, but it deletes the packing
C     information for the given source number and compacts the read
C     buffer, adjusting the pointers in the packing information block
C     for the other files.
C
C     This routine will also flush the current file buffer to disc
C     if necessary.
C
C     NPR,    June 1987.
C-
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_save.inc'
      include '/mrao/post/include/sf_buffer.inc'
      include '/mrao/post/include/samplib_errors.inc'

C     Loop counters
      integer     i, j
C     Position in SF_SAVE of packing information to be deleted
      integer     sf_save_no
C     Position of last sample file in SF_SAVE
      integer     last_sf
C     Start of last buffer
      integer     last_buff

      if ( s .ne. 0 ) return

C     Call write_buffer to ensure that the last buffer is written to
C     disc if it has been updated.  This call also restores the packing
C     information.

      call write_buffer( lun, src_num, s )

C     Find the packing information.
      j = 0
      sf_save_no = 0
      last_sf    = max_sf

  10  if ((j .eq. max_sf) .or. (last_sf .ne. max_sf)) goto 20
          j = j+1
          if (sf_save(lun_ptr,j) .eq. 0) then
              last_sf = j-1
          else if (sf_save(lun_ptr,j) .eq. sf_lun .and.
     *             sf_save(src_ptr,j) .eq. sf_src_num   )  then
              sf_save_no = j
          end if
      goto 10
  20  continue

      if (sf_save_no .eq. 0) then
          s = ILL_SFPACK
          goto 999
      end if
      last_buff = sf_save( start_ptr, last_sf )

C     Compact file read buffer
      do 30, i = buffer_ptr, last_buff - 1
          buffer( i ) = buffer( i+buffer_len )
  30  continue

C     Compact SF_SAVE and update buffer pointers.
      do 50, i = sf_save_no, last_sf-1
          do 40, j = 1, sf_pack_len
              sf_save( j, i ) = sf_save( j, i+1 )
  40      continue
          sf_save(start_ptr,i) = sf_save(start_ptr,i)-buffer_len
  50  continue

C     Set SF_PACK and the last sample file in SF_SAVE invalid.
      do 60, i = 1, sf_pack_len
          sf_save( i, last_sf ) = 0
          sf_pack( i ) = 0
  60  continue

      if (s.ne.0) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine CLOSE_SOURCE' )

      end
