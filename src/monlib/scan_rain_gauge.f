*+SCAN_RAIN_GAUGE

       subroutine scan_rain_gauge (ifile, isamp1, isamp2, ivar, log,
     :                                                    rmax, status)
C      ----------------------------------------------------------------
C
C  Scans rain gauge readings within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      IVAR          integer       selects display variable
C      LOG           logical       log fluctuations control flag
C      RMAX          real          log fluctuations > RMAX
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only].
C
C  Subroutine to scan the sample file currently opened on logical unit
C  IFILE, and report the rain gauge readings recorded during the observation,
C  for the aerials included in the given aerial list.  A table is produced
C  of the minimum, maximum and mean readings within the specified sample
C  range.
C
C  Changes in the readings during the run may also be logged.
C
C  If IVAR=1, the rain gauge readings themselves are displayed.
C  If IVAR=2, the derived rain correction factors are displayed.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 3/4/90]
*-
       integer    ifile, isamp1, isamp2, ivar, status
       real       rmax
       logical    log
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  stime*8
       integer    i, iae, isamp, n, nae
       integer    itime(3), isecs, iout, length
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
       logical    report
c
c  Workspace
c
       real*8     rain_mean(max_aes), rain_rms(max_aes)
       real*4     rain_min(max_aes), rain_max(max_aes)
       real*4     rain(max_aes), value(max_aes)
c
       common /post/ rain_mean, rain_rms, rain_min, rain_max, rain,
     :               value
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       nae = max_aes
       do iae=1,nae
         rain_min(iae)=10.0
         rain_max(iae)=-10.0
         rain_mean(iae)=0.d0
         rain_rms(iae)=0.d0
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
c    Report fluctuations greater than RMAX
c
             if (ivar.eq.1) then
               call enq_mon_rain( ifile, iae, value(iae), status )
             elseif (ivar.eq.2) then
               call enq_mon_rfac( ifile, iae, value(iae), status )
             endif
             if (abs(rain(iae)-value(iae)).gt.rmax) then
               if (log) report=.true.
             endif
c
c    Accumulate min, max, sum, sum**2
c
             if (value(iae).lt.rain_min(iae)) then
               rain_min(iae)=value(iae)
             elseif (value(iae).gt.rain_max(iae)) then
               rain_max(iae)=value(iae)
             endif
             rain_mean(iae)=rain_mean(iae)+value(iae)
             rain_rms(iae)=rain_rms(iae)+value(iae)*value(iae)
           enddo
           n=n+1
         endif
c
         if (report) then
           do iae=1,nae
             rain(iae)=value(iae)
           enddo
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,stime,length)
           write(iout,1)isamp,stime,(rain(iae),iae=1,nae)
    1      format(' sample',I5,2X,A,' ST',2X,8F6.2)
           if (io_attn(status)) return
         endif
c
       enddo
c
c    Print out min, max, mean, rms by aerial
c
       if (n.gt.0 .and. .not.log) then
         do iae=1,nae
           rain_mean(iae)=rain_mean(iae)/n
           rain_rms(iae)=rain_rms(iae)/n-rain_mean(iae)*rain_mean(iae)
           rain_rms(iae)=dsqrt(dmax1(0.d0,rain_rms(iae)))
         enddo
         if (ivar.eq.1) then
           write(iout,2)n,isamp1,isamp1+n-1
         elseif (ivar.eq.2) then
           write(iout,3)n,isamp1,isamp1+n-1
         endif
         do iae=1,nae
           write(iout,4)iae,rain_min(iae),rain_max(iae),
     :                      rain_mean(iae),rain_rms(iae)
         enddo
c
    2    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean rain gauge readings:'/X,34('-'))
    3    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean rain correction factors:'/X,39('-'))
    4    format(2X,'ae',I3,2X,2(2F7.2,F7.1,'  (',F4.1,')'))
c
       endif
c
       end


