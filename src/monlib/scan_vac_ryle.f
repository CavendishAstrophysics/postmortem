*+SCAN_VAC_RYLE

       subroutine scan_vac_ryle (ifile, isamp1, isamp2, log, rmax,
     :                                                           status)
C      -----------------------------------------------------------------
C
C  Scans vacuum readings within sample file.
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
C  and report the vacuum readings recorded during the observation, for the
C  aerials included in the given aerial list.  A table is produced of the
C  minimum, maximum and mean ALC values within the specified sample range.
C
C  Changes in the readings during the run may also be logged.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 23/9/94]
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
       real*8     vac_mean(max_aes), vac_rms(max_aes)
       real*4     vac_min(max_aes), vac_max(max_aes)
       real*4     vac(max_aes), value(max_aes)
c
       common /post/ vac_mean, vac_rms, vac_min, vac_max, vac, value
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       nae = max_aes
       do iae=1,nae
         vac(iae)=0.0
         vac_min(iae)=100.0
         vac_max(iae)=-100.0
         vac_mean(iae)=0.d0
         vac_rms(iae)=0.d0
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
             call enq_mon_vac(ifile,iae,value(iae),status)
             if (abs(vac(iae)-value(iae)).gt.rmax) then
               if (log) report=.true.
             endif
c
c    Accumulate min, max, sum, sum**2
c
             if (value(iae).lt.vac_min(iae)) then
               vac_min(iae)=value(iae)
             elseif (value(iae).gt.vac_max(iae)) then
               vac_max(iae)=value(iae)
             endif
             vac_mean(iae)=vac_mean(iae)+value(iae)
             vac_rms(iae)=vac_rms(iae)+value(iae)*value(iae)
           enddo
           n=n+1
         endif
c
         if (report) then
           do iae=1,nae
             vac(iae)=value(iae)
           enddo
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,stime,length)
           write(iout,1)isamp,stime(1:length),(vac(iae),iae=1,nae)
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
           vac_mean(iae)=vac_mean(iae)/n
           vac_rms(iae)=vac_rms(iae)/n-vac_mean(iae)*vac_mean(iae)
           vac_rms(iae)=dsqrt(dmax1(0.d0,vac_rms(iae)))
         enddo
         write(iout,2)n,isamp1,isamp1+n-1
         do iae=1,nae
           write(iout,3)iae,vac_min(iae),vac_max(iae),
     :                      vac_mean(iae),vac_rms(iae)
         enddo
c
    2    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean vacuum readings:'/X,30('-'))
    3    format(2X,'ae',I3,2X,2(2F8.2,F8.1,'  (',F4.1,')'))
c
       endif
c
       end


