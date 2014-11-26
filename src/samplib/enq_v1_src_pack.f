

C+ENQ_V1_SRC_PACK

      subroutine enq_v1_src_pack ( lun, src_num, src_pack_record, s )
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
C     tables versions 0 and 1.
C-

C     Global includes -
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/control_tables.inc'
      include  '/mrao/post/include/control_tab_v0.inc'
      include  '/mrao/post/include/ctab_pack.inc'
      include  '/mrao/post/include/src_pack.inc'
      include  '/mrao/post/include/samplib_errors.inc'

      integer     integ_time, stop_ut, start_ut
      integer     offset, sf_type, obs_flag
      integer     i, itime(3)

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      call enq_sftype( lun, sf_type, s )

      if (ct_vers.eq.0 .and. (sf_type.eq.1 .or. sf_type.eq.2)) then

C         Old style control tables - check to see source number is OK.
          if ( .not. ((src_num .ge. 1) .and.
     *                ((sf_type.eq.1 .and. src_num.eq.1 ) .or.
     *                 (sf_type.eq.3 .and. src_num.le.nrem)   ))) then
              s = NO_SRCNUM
              goto 999
          end if

C         Do the things which are independent of sf_type first
          src_pack_type  = 1
          src_samp_len   = lrecb/2
          src_start_rt   = 1
          src_length_rt  = 8
          src_start_vis  = src_start_rt+src_length_rt
          src_max_vis    = nsp
          src_data_type  = 1
          src_amp_factor = 32767.0 / ( achop*ampscl )
          src_start_time = 10*(istim1(1) + 60*(istim1(2)+60*istim1(3)))
          src_stop_time  = 10*(istim2(1) + 60*(istim2(2)+60*istim2(3)))
          src_interp_type= 2

C         Now the things which are dependant on sf_type
          if ( sf_type.eq.1 ) then
              src_samp_ptr  = ictab*page
              src_num_samp  = nsamp
              src_start_mon = src_start_vis + src_max_vis + 10
              src_length_mon= ( 6*max_aes + 10 ) / 2

              integ_time = 2*intsam*(itab(1)+itab(2))/1000

C             Readjust SRC_NUM_SAMP if run ISTOP flag is not set.
              if ( istop.eq.0 ) then
*                 call enq_obsstat(obs_flag)
                  if ( obs_flag .eq. 2 ) then
C                    Observation in progress - Nominal stop time is now.
                      call util_enqtim(itime)
                      stop_ut = 10*(itime(1)+60*(itime(2)+60*itime(3)))
                  else
C                     Run aborted - stop time is predicted end of run.
                      stop_ut = 10*(itim2(1)+60*(itim2(2)+60*itim2(3)))
                  end if

                  start_ut = 10*(itim1(1) + 60*(itim1(2)+60*itim1(3)))
                  if (stop_ut .lt. start_ut) stop_ut=stop_ut + 864000
c                 src_num_samp = (stop_ut-start_ut)/(2*intsam*isamps)
                  src_num_samp = (stop_ut-start_ut)/integ_time
                  src_stop_time= src_start_time +
     *                           (stop_ut-start_ut)*const_sut2sst
              end if

C             Correct the stop time if necessary.
  10          if ((src_stop_time-src_start_time)       .lt.
c    *            (src_num_samp*2*intsam*isamps-432000)     ) then
     *            (src_num_samp*integ_time-432000)     ) then
                  src_stop_time = src_stop_time + 864000
              goto 10
              end if
          else if ( sf_type.eq.3 ) then
              src_samp_ptr = ictab*page
              do 20, i = 1, (src_num-1)
                 src_samp_ptr = src_samp_ptr+(lrecb/2)*nsampr(i)
  20          continue
              src_num_samp   = nsampr(src_num)
              src_samp_len   = lrecb/2
              src_start_mon  = 0
              src_length_mon = 0

              integ_time = 2*intsam*isamps
C             Correct the stop time if necessary.
  30          if ((src_stop_time-src_start_time) .lt.
c    *            (src_num_samp*2*intsam*isamps-432000)     ) then
     *            (src_num_samp*integ_time-432000)     ) then
                  src_stop_time = src_stop_time + 864000
              goto 30
              end if
          end if

          do 40, i = 1, src_pack_len
              src_pack_record(i) = src_pack(i)
  40      continue

      else if (ct_vers .gt. 0) then
          call enq_ctab_pack( lun, ctab_pack, s )
          if ( s .ne. 0 ) goto 999

          if ( 1 .le. src_num .and. src_num .le. ct_num_src ) then
              offset = ct_src_ptr + (src_num-1)*ct_len_src
              do 50, i = 1, ct_len_pack
                  src_pack_record(i) = ct_all( offset+i )
  50          continue
          else
              s = NO_SRCNUM
          end if
      else
          s = ILL_CONTTAB
      end if

      if (s .ne. 0) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_V1_SRC_PACK' )

      end
