

*+SCAN_AGCS_CLFST

       subroutine scan_agcs_clfst (ifile, list, ilist, nae,
     :                                isamp1, isamp2, log, rmax, status)
C      -----------------------------------------------------------------
C
C  Scans AGC readings within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      LOG           logical       log AGC changes control flag
C      RMAX          real          log AGC changes > RMAX
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to scan the sample file currently opened on logical unit IFILE,
C  and report the AGC readings recorded during the observation, for the
C  aerials included in the given aerial list.  A table is produced of the
C  minimum, maximum and mean AGC values within the specified sample range.
C
C  Changes in the AGC values during the run may also be logged.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, modified by PA for V1 and V2 CT, 9/11/88]
*-
       character  list*(*)
       integer    ifile, nae, ilist(nae)
       integer    isamp1, isamp2, status
       real       rmax
       logical    log
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/mon_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  line*40, stime*12
       integer    itime(3), isecs, iout, n
       integer    i, iae, iae1, iae2, ihut, isamp, length, mh
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
       logical    report
       logical    test
c
c  Workspace
c
       real*8     agc_mean(max_aes), agc_rms(max_aes)
       real*4     agc_min(max_aes), agc_max(max_aes)
       real*4     agc(max_aes), value(max_aes)
       integer    iagc(max_aes)
c
       common /post/ agc_mean, agc_rms, agc_min, agc_max, agc, iagc,
     :               value
c
c
       if (status.ne.0) return
c
       stime(9:12)=' ST '
       call io_enqout(iout)
c
       do iae=1,max_aes
         agc(iae)=0.0
         agc_min(iae)=max_par
         agc_max(iae)=-max_par
         agc_mean(iae)=0.d0
         agc_rms(iae)=0.d0
       enddo
c
       if (log) then
         if (nae.le.10) then
           write(iout,'(/23X,A,10I5)')'aerials',(ilist(i),i=1,nae)
           write(iout,*)
         else
           write(iout,'(/33X,A,A/)')'aerials : ',list(1:chr_lenb(list))
         endif
       endif
c
       n=0
       do isamp=isamp1,isamp2
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                          samp_sid_time,samp_wt,status)
         if (status.eq.0) then
           report=.false.
c
c  For each aerial in the list
c
           do i=1,nae
             iae=ilist(i)
             call enq_aestat( ifile, iae, 1, test, status )
             if (test) then
c
c    Report AGC changes greater than RMAX
c
               call enq_mon_agc( ifile, iae, value(iae), status )
               if (abs(agc(iae)-value(iae)).gt.rmax) then
                 if (log) report=.true.
               endif
c
c    Accumulate min, max, sum, sum**2
c
               if (value(iae).lt.agc_min(iae)) then
                 agc_min(iae)=value(iae)
               elseif (value(iae).gt.agc_max(iae)) then
                 agc_max(iae)=value(iae)
               endif
               agc_mean(iae)=agc_mean(iae)+value(iae)
               agc_rms(iae)=agc_rms(iae)+value(iae)*value(iae)
             endif
           enddo
           n=n+1
c
           if (report) then
             do iae=1,max_aes
               agc(iae)=value(iae)
               iagc(iae)=int(agc(iae))
             enddo
             isecs=samp_sid_time/10
             call util_stohms(isecs,itime)
             call chr_chtime(itime,stime(1:8),length)
             write(iout,1)isamp,stime,(iagc(ilist(i)),i=1,nae)
    1        format(' sample',I5,2X,A,4X,10I5/(30X,10I5))
             if (io_attn(status)) return
           endif
c
         endif
       enddo
c
c    Print out min, max, mean, rms by hut or aerial
c
       if (n.gt.0 .and. .not.log) then
         do i=1,nae
           iae=ilist(i)
           agc_mean(iae)=agc_mean(iae)/n
           agc_rms(iae)=agc_rms(iae)/n-agc_mean(iae)*agc_mean(iae)
           agc_rms(iae)=dsqrt(dmax1(0.d0,agc_rms(iae)))
         enddo
         write(iout,2)n,isamp1,isamp1+n-1
         call enq_groups(ifile,mh,status)
         do ihut=1,mh
           n=0
           call enq_ae_hut(ifile,ihut,iae1,iae2,status)
           do iae=iae1,iae2
             do i=1,nae
               if (iae.eq.ilist(i)) then
                 n=n+1
                 call enq_aestat(ifile, iae, 1, test, status)
                 if (test) then
                   write(line,3)iae,agc_min(iae),agc_max(iae),
     :                              agc_mean(iae),agc_rms(iae)
                 else
                   write(line,4)iae
                 endif
                 if (n.eq.1) write(iout,*)
                 write(iout,*)line
               endif
             enddo
           enddo
           if (io_attn(status)) return
         enddo
c
    2    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean AGC values:'/X,25('-'))
    3    format(2X,'ae',I3,2X,2(3F7.0,'  (',F4.1,')'))
    4    format(2X,'ae',I3,2X,4('     . '))
c
       endif
c
       end
