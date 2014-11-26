

C+PUT_SF_PACK

      subroutine put_sf_pack ( s )
C
C     Writes sample file packing parameters.

C     Given:
C         None.

C     Returned:
C         Status - must be zero on entry.
              integer     s
C
C     This routine saves the packing parameters in the common block
C     SF_SAVE. They can be restored by GET_SF_PACK.
C
C-
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_save.inc'
      include '/mrao/post/include/samplib_errors.inc'

      logical     done
      integer     i, j

      if (s.ne.0) return

      if ((sf_lun .le. 1) .or. (sf_src_num .le. 0)) then
          s = ILL_SFPACK
          goto 999
      end if

      j = 1
      done = .false.

  10  if (done .or. (j .gt. max_sf)) goto 20
         if ((sf_save(lun_ptr,j).eq.sf_lun .and.
     *      sf_save(src_ptr,j).eq.sf_src_num   ) .or.
     *      sf_save(lun_ptr,j).eq.0                    ) then
            do i = 1, sf_pack_len
               sf_save(i,j) = sf_pack(i)
            enddo
            done = .true.
         else
            j = j+1
         end if
      goto 10
  20  continue

      if (.not. done) then
         s = TOO_MANYSF
         goto 999
      end if
      return

 999  call smp_wrerr( s, 'in subroutine PUT_SF_PACK' )

      end
