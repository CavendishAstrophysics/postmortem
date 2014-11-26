
C+lsf_archive_flux

      SUBROUTINE lsf_archive_flux (lsf_num, s)

C  write a single record to the archive file with the
C     source name, date, times, flux-density, comment etc

C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C GGP    1 Feb 2002
C       28 May 2003 : check previous entries for that file
C        9 Oct 2003 : one more digit on mean flux 
C-
C     ****************************************************************

C   Function declarations, global includes

      include  '/mrao/include/chrlib_functions.inc'

      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/iolib_functions.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/post/include/merge_types.inc'


        real*8          mean, mean_sq, ms_dev, se_mean

        integer         iout, nspac, no_group
        integer         sid1(3), sid2(3)
        integer         date1(3), time1(3), date2(3), time2(3)

        real*8          deltaT, MJD0, MJDmean

       character  list*80, source*20, line*80
       character  full_name*120, short_name*16, dir_name*100, type*4
       character  samp_dir*100
       character  archive_f*80
       parameter (archive_f = '/data/ryle/tel/flux_archive')
       integer    archive_u
       integer    ibuff1, ibuff2, sf_lun, src_num
       integer    iout, merge_type
       integer    n, nspac, no_group

       integer    ae_list(max_aes)
       integer    no_samp(max_spac),     ilist(2*max_spac)
       integer    group_size(2*max_vis), group_list(2*max_vis)
       real*8     acc(4,max_vis),        acc_sq(4,max_vis)
       real*4     values(max_vis),       rms(max_vis)

       common  /post/  ae_list, no_samp, ilist,
     :                 group_size, group_list,
     :                 acc, acc_sq, values, rms


C     ****************************************************************
C
C
C     Subroutine initialisation
C
      if (s .ne. 0) return

      if (lsf_num .ne. curr_lsf_num) then
          call get_lsf(lsf_num, s)
          if (s .ne. 0) goto 9999
      end if

C         Main Code - mainly derived from scan_sample_ryle
C         ---------
C
        call io_enqout (iout)

C  Enquire sample file info and output unit
       call lsf_enq_sf(lsf_num, sf_lun, src_num, s)
       inquire (sf_lun, name = full_name)
        call io_brkfil (full_name, dir_name, short_name, type)
        call io_brkfil (dir_name, samp_dir, short_name, type)
*        write (*,*) full_name(1:chr_lenb(full_name))
*        write (*,*) samp_dir(1:chr_lenb(samp_dir))
*        write (*,*) short_name(1:chr_lenb(short_name))
*        write (*,*) type

C  Prompt for spacing list and list of spacings to scan
         call get_spacings(sf_lun, 'Scan spacings : ', 'all',
     :                      list, sp_list, max_vis, nspac, s)
         call lsf_set_spacings(lsf_num, nspac, sp_list, 2, s)
         call chr_chucas(list)

        merge_type = total_merge

C  Define merge list using merge-type total_merge ...
         call set_merge(sf_lun, sp_list, nspac, merge_type,
     :                  ilist, no_group, group_size, s)

C  Get range of sample buffers to scan
         call lsf_get_range(lsf_num, ibuff1, ibuff2, s)


C  Accumulate data (note the use of ilist as opposed to group_list here)
         call scan_samples(sf_lun,lsf_num,sp_list,nspac,ibuff1,ibuff2,
     :                                      ilist,no_group,group_size,
     :                                        acc,acc_sq,no_samp,s)

* only one group (total-merge), only use cosine data

        N       = no_samp(1)
        mean    = acc(1,1)/N
        mean_sq = acc_sq(1,1)/N

        ms_dev  = mean_sq - mean*mean

        se_mean = dsqrt(abs(ms_dev))

        call enq_sid_time(sid1, sid2, s)
        deltaT = (sid2(3)+sid2(2)/60.0) - (sid1(3)+sid1(2)/60.0)
        if (deltaT .lt. 0.0) deltaT = deltaT + 24.0d0

        call enq_mjdstart (mjd0, s)
        MJDmean = MJD0 + (0.5*deltaT)/(24.0D0*const_sut2sst)
        source = ' '
        call enq_src_name (sf_lun, src_num, source, s)

* date as d,m,y; time as s,min,h

        call enq_loc_time (sf_lun, date1, time1, date2, time2, s)

*        write (iout, *) 'date/times', date1,time1, date2,time2

           write (iout, 8101)
     *      short_name, source, MJDmean, mean, se_mean, N

* check existing entries

        call io_opefil (archive_u, archive_f, 'READ', 0, s)

        do while (s .eq. 0)

                read (archive_u, '(a)', end = 400) line

                if (short_name .eq. line(1:16)) then
                    write (iout, *) 'existing entry for sample file:'
                    write (iout, *) line (1:chr_lenb(line))
                    if (.not. io_YesNo('continue ?','yes',s))goto 500
                endif
        enddo
  400   close (archive_u)



        call io_opefil (archive_u, archive_f, 'WA', 0, s)

        if (s .eq. 0) then
           write (archive_u, 8100)
     *        short_name, source, MJDmean, mean, se_mean, N
        endif

  500   close (archive_u)

      if (s.ne.0) goto 9999
      return

8100    format  (  a, 2x, a, x, f10.2, x, f9.5,  f8.4, x, i4)
8101    format  (x,a, 2x, a, x, f10.2, x, f9.5,  f8.4, x, i4)

* formerly  ............................. f8.4,x,f8.4 ...

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call lsf_wrerr(s, 'in subroutine LSF_archive_flux')
          end if
          return
      end
