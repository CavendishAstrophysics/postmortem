*+SCAN_CRYO_RYLE

       subroutine scan_cryo_ryle (ifile, isamp1, isamp2, log, rmax,
     :                                                           status)
C      -----------------------------------------------------------------
C
C  Scans cryo temperature readings within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      LOG           logical       log changes control flag
C      RMAX          real          log changes > RMAX
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE telescope only]
C
C  Subroutine to scan the sample file currently opened on logical unit IFILE,
C  and report the cryo temperature readings recorded during the observation,
C  for the aerials included in the given aerial list.  A table is produced of
C  the minimum, maximum and mean ALC values within the specified sample range.
C
C  Changes in the readings during the run may also be logged.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 15/2/94]
*-
       integer    ifile, isamp1, isamp2, status
       real       rmax
       logical    log
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  stime*12
       integer    i, iae, isamp, n, nae
       integer    itime(3), isecs, iout, length
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
       logical    report
c
c  Workspace
c
       real*8     tc_mean(max_aes), tc_rms(max_aes)
       real*4     tc_min(max_aes), tc_max(max_aes)
       real*4     tc(max_aes), value(max_aes)
c
       common /post/ tc_mean, tc_rms, tc_min, tc_max, tc, value
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       nae = max_aes
       do iae=1,nae
         tc(iae)=0.0
         tc_min(iae)=100.0
         tc_max(iae)=-100.0
         tc_mean(iae)=0.d0
         tc_rms(iae)=0.d0
       enddo
c
       if (log) then
         write(iout,'(/20X,A,8I6)')'aerials',(i,i=1,nae)
         write(iout,*)
       endif
c
       n=0
       do isamp=isamp1,isamp2
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                          samp_sid_time,samp_wt,status)
         if (status.eq.0) then
c
c  For each aerial
c
           report=.false.
           do iae=1,nae
c
c    Report changes greater than RMAX
c
             call enq_mon_cryo(ifile,iae,value(iae),status)
             if (abs(tc(iae)-value(iae)).gt.rmax) then
               if (log) report=.true.
             endif
c
c    Accumulate min, max, sum, sum**2
c
             if (value(iae).lt.tc_min(iae)) then
               tc_min(iae)=value(iae)
             elseif (value(iae).gt.tc_max(iae)) then
               tc_max(iae)=value(iae)
             endif
             tc_mean(iae)=tc_mean(iae)+value(iae)
             tc_rms(iae)=tc_rms(iae)+value(iae)*value(iae)
           enddo
           n=n+1
         endif
c
         if (report) then
           do iae=1,nae
             tc(iae)=value(iae)
           enddo
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,stime,length)
           write(iout,1)isamp,stime(1:length),(tc(iae),iae=1,nae)
    1      format(' sample',I5,2X,A,' ST',2X,8F6.1)
           if (io_attn(status)) return
         endif
c
       enddo
c
c    Print out min, max, mean, rms by hut or aerial
c
       if (n.gt.0 .and. .not.log) then
         do iae=1,nae
           tc_mean(iae)=tc_mean(iae)/n
           tc_rms(iae)=tc_rms(iae)/n-tc_mean(iae)*tc_mean(iae)
           tc_rms(iae)=dsqrt(dmax1(0.d0,tc_rms(iae)))
         enddo
         write(iout,2)n,isamp1,isamp1+n-1
         do iae=1,nae
           write(iout,3)iae,tc_min(iae),tc_max(iae),
     :                      tc_mean(iae),tc_rms(iae)
         enddo
c
    2    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean cryo temperatures:'/X,32('-'))
    3    format(2X,'ae',I3,2X,2(2F8.2,F8.1,'  (',F4.1,')'))
c
       endif
c
       end


