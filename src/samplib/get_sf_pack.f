

C+GET_SF_PACK

      subroutine get_sf_pack ( lun, src_num, s )
C
C     Reads sample file packing parameters after saving the current set.
C
C     Given:
C         Logical unit number of sample file.
              integer     lun
C         Source number in sample file.
              integer     src_num
C
C     Returned:
C         Status - must be zero on entry
              integer     s
C
C     This routine saves the current set of packing parameters and
C     restores the packing parameters to common for the source number
C     SRC_NUM contained in the sample file opened on unit LUN.
C
C-

      logical     found
      integer     i, j

      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_save.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if (s.ne.0) return

C     If the current packing parameters are valid, save them

      if ((sf_lun .gt. 1) .and. (sf_src_num .ge. 1)) then
          call put_sf_pack( s )
          if (s.ne.0) goto 999
      end if

      j = 1
      found = .false.

  10  if ( found .or. (j .gt. max_sf) ) goto 20
          if ((sf_save(lun_ptr,j) .eq. lun)     .and.
     *        (sf_save(src_ptr,j) .eq. src_num)       ) then
              do i = 1, sf_pack_len
                 sf_pack(i) = sf_save(i,j)
              enddo
              found = .true.
          else
              j = j + 1
          end if
      goto 10
  20  continue

      if ( .not. found ) then
          s = SF_NOTSAV
          goto 999
      end if
      return

 999  call smp_wrerr( s, 'in subroutine GET_SF_PACK' )

      end
