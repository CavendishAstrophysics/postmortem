

C+ENQ_SRC_DEF

       subroutine enq_src_def ( lun, src_num, src_def_record, s )
C
C     Returns source definition information for a sample file & source.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         The source number in the sample file
              integer         src_num
C
C     Returned:
C         Source definition record.
              integer         src_def_record(*)
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     This routine can only be used for remove and calibration sources.
C     The record definitions returned are in:
C
C         /mrao/post/include/remove_record.inc'
C         /mrao/post/include/cal_record.inc'
C
C     Possible return status's are:
C         ILL_CONTTAB - wrong version of control tables for this routine
C
C-

C     Global includes -
C
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/ctab_pack.inc'
      include '/mrao/post/include/remove_record.inc'
      include '/mrao/post/include/samplib_errors.inc'

      integer     i, offset, sf_type

      if ( s .ne. 0 ) return
      call read_ct ( lun, s )
      call enq_sftype( lun, sf_type, s )

      if (ct_vers .eq. 0 .and. sf_type .eq. 2) then
C         Old remove file - initialise remove record.
          if ( src_num .ge.1 .and. src_num .le. nrem ) then
              do 10, i = 1, rem_length
                  remove_record(i) = 0
  10          continue
              rem_ion     = limrem(src_num)
              rem_refdat  = datobs
              rem_ra(1)   = rem(1, src_num)
              rem_dec(1)  = rem(2, src_num)
              rem_source  = rsource(src_num)
              do 20, i = 1, rem_length
                  src_def_record(i) = remove_record(i)
  20          continue
          else
              s = NO_SOURCE
              goto 999
          end if
      else
          call enq_ctab_pack( lun, ctab_pack, s )
          if ( s .ne. 0 ) goto 999
          if ( 1 .le. src_num .and. src_num .le. ct_num_src ) then
              offset = ct_src_ptr + (src_num-1)*ct_len_src + ct_len_pack
              do 30, i = 1, ct_len_src-ct_len_pack
                  src_def_record(i) = ct_all( offset+i )
  30          continue
          else
              s = NO_SOURCE
              goto 999
          end if
      end if

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_SRC_DEF' )

      end
