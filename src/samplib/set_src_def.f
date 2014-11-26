

C+SET_SRC_DEF

       subroutine set_src_def ( lun, src_num, src_def_record, s )
C
C     Sets source packing information for a sample file and source.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         The source number in the sample file
              integer         src_num
C         Source definition record.
              integer         src_def_record(*)

C     Returned:
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Possible return status's are:
C         ILL_CONTTAB - wrong version of control tables for this routine
C
C-

C     Global includes -
C
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/ctab_pack.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer     i, offset

      if ( s .ne. 0 ) return

      call read_ct ( lun, s )
      call enq_ctab_pack( lun, ctab_pack, s )
      if ( s .ne. 0 ) goto 999

      if ( 1 .le. src_num .and. src_num .le. ct_num_src ) then
          offset = ct_src_ptr + (src_num-1)*ct_len_src + ct_len_pack
          do 10, i = 1, ct_len_src-ct_len_pack
              ct_all( offset+i ) = src_def_record(i)
  10      continue
          call write_ct( lun, s )
      else
          s = NO_SOURCE
      end if
      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine SET_SRC_DEF' )

      end
