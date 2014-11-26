
C+CLOSE_SF

      subroutine close_sf ( lun, s )
C
C     Closes a sample file and all sources associated with it.
C
C     Given:
C         Logical unit number of sample file
              integer         lun

C     Returned:
C         Status - must be zero on entry.
              integer         s
C
C     This routine calls CLOSE_SOURCE for every source open associated
C     with the given sample file, then closes the file itself.
C
C     If the current control tables belong to this unit number, the
C     current control tables unit number is set to zero.
C
C     NPR,    June 1987
C-
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/sf_save.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

C    Loop counters
      integer     i
C    Source number to close.
      integer     source
C    Position of last sample file in SF_SAVE
      logical     last_sf

      if ( s .ne. 0 ) return

C    Find the open sources and close them.
      i = 1
      last_sf = .false.

  10  if ((i .gt. max_sf) .or. last_sf ) goto 20
          if (sf_save(lun_ptr,i) .le. 0 ) then
              last_sf = .true.
          else if (sf_save(lun_ptr,i) .eq. lun) then
              source = sf_save( src_ptr, i )
              call close_source( lun, source, s )
              if ( s .ne. 0 ) goto 999
          else
              i = i+1
          end if
      goto 10
  20  continue

      call io_close ( lun, s )
      if ( lun .eq. ct_lun )  ct_lun  = 0
      if ( lun .eq. ion_lun ) ion_lun = 0
      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine CLOSE_SF' )

      end
