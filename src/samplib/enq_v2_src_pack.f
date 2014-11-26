

C+ENQ_V2_SRC_PACK

      subroutine enq_v2_src_pack ( lun, src_num, src_pack_record, s )
C
C     Returns source packing information for a given sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         The source number in the sample file
              integer         src_num

C     Returned:
C         Source packing record - see 'src_pack.inc'
              integer         src_pack_record(*)
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Possible return status's are:
C         ILL_CONTTAB - wrong version of control tables for this routine
C
C     This routine is a support routine for ENQ_SRC_PACK for control
C     tables version 2.
C-

C     Global includes -
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/control_tables.inc'
      include  '/mrao/post/include/control_tab_v2.inc'
      include  '/mrao/post/include/ctab_pack.inc'
      include  '/mrao/post/include/src_pack.inc'
      include  '/mrao/post/include/samplib_errors.inc'

      integer     i, offs, sf_type, itime(3), stop_ut, start_ut

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      call enq_sftype( lun, sf_type, s )

      if (ct_vers .eq. 2) then
          call enq_ctab_pack( lun, ctab_pack, s )
          if ( s .ne. 0 ) goto 999

          if ( 1 .le. src_num .and. src_num .le. ct_num_src ) then
              offs = ct_src_ptr + (src_num-1)*ct_len_src
              do 10, i = 1, ct_len_pack
                  src_pack(i) = ct_all( offs+i )
  10          continue

C             Readjust SRC_NUM_SAMP if run ISTOP flag is not set.
              if ( sf_type.eq.1 .and.
     :            ( istart.eq.1 .and. istop.eq.0 )) then
C                Observation in progress - Nominal stop time is now.
                  call util_enqtim(itime)
                  stop_ut = 10*(itime(1)+60*(itime(2)+60*itime(3)))
                  start_ut = 10*(itim1(1)+60*(itim1(2)+60*itim1(3)))
                  if (stop_ut .lt. start_ut) stop_ut = stop_ut + 864000
                  src_stop_time = src_start_time +
     :                               (stop_ut-start_ut)*const_sut2sst
                  if (src_stop_time .lt. src_start_time)
     :                           src_stop_time = src_stop_time + 864000
                  src_num_samp =
     :                 (src_stop_time-src_start_time)/(10*integration)
              end if

              do 20, i = 1, src_pack_len
                  src_pack_record(i) = src_pack(i)
  20          continue

          else
              s = NO_SRCNUM
          end if
      else
          s = ILL_CONTTAB
      end if

      if (s .ne. 0) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_V2_SRC_PACK' )

      end
