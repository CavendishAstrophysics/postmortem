*+SCAN_MISC_RYLE

       subroutine scan_misc_ryle (ifile, ilist, npar, isamp1, isamp2,
     :                                                log, rmax, status)
C      -----------------------------------------------------------------
C
C  Scans miscellaneous parameter readings within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ILIST         integer(*)    list of parameter indexes
C      NPAR          integer       number of parameters in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      LOG           logical       log fluctuations control flag
C      RMAX          real          log fluctuations > RMAX
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only].
C
C  Subroutine to scan the sample file currently opened on logical unit
C  IFILE, and report selected parameters recorded during the observation.
C  A table is produced of the minimum, maximum and mean readings within
C  the specified sample range.
C
C  Changes in the readings during the run may also be logged.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 22/2/93]
*-
       integer    ifile, npar, isamp1, isamp2, status
       integer    ilist(npar)
       real       rmax
       logical    log
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  stime*8
       integer    i, ipar, isamp, n
       integer    itime(3), isecs, iout, length
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
       logical    report
c
c  Workspace
c
       integer    max_pars
       parameter (max_pars=3*max_aes)
       real*8     par_mean(max_pars), par_rms(max_pars)
       real*4     par_min(max_pars), par_max(max_pars)
       real*4     par(max_pars), value(max_pars)
c
       common /post/ par_mean, par_rms, par_min, par_max, par,
     :               value
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       do ipar=1,npar
         par_min(ipar)=10.0
         par_max(ipar)=-10.0
         par_mean(ipar)=0.d0
         par_rms(ipar)=0.d0
       enddo
c
       if (log) then
         write(iout,'(/17X,A,8I6)')'parameters',(ilist(i),i=1,npar)
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
           do ipar=1,npar
c
c    Report fluctuations greater than RMAX
c
             call enq_mon_misc( ifile, ilist(ipar), value(ipar), status)
             if (abs(par(ipar)-value(ipar)).gt.rmax) then
               if (log) report=.true.
             endif
c
c    Accumulate min, max, sum, sum**2
c
             if (value(ipar).lt.par_min(ipar)) then
               par_min(ipar)=value(ipar)
             elseif (value(ipar).gt.par_max(ipar)) then
               par_max(ipar)=value(ipar)
             endif
             par_mean(ipar)=par_mean(ipar)+value(ipar)
             par_rms(ipar)=par_rms(ipar)+value(ipar)*value(ipar)
           enddo
           n=n+1
         endif
c
         if (report) then
           do ipar=1,npar
             par(ipar)=value(ipar)
           enddo
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,stime,length)
           write(iout,1)isamp,stime,(par(ipar),ipar=1,npar)
    1      format(' sample',I5,2X,A,' ST',2X,8F6.2)
           if (io_attn(status)) return
         endif
c
       enddo
c
c    Print out min, max, mean, rms by aerial
c
       if (n.gt.0 .and. .not.log) then
         do ipar=1,npar
           par_mean(ipar)=par_mean(ipar)/n
           par_rms(ipar)=par_rms(ipar)/n-par_mean(ipar)*par_mean(ipar)
           par_rms(ipar)=dsqrt(dmax1(0.d0,par_rms(ipar)))
         enddo
         write(iout,2)n,isamp1,isamp1+n-1
         do ipar=1,npar
           write(iout,3)ilist(ipar),par_min(ipar),par_max(ipar),
     :                              par_mean(ipar),par_rms(ipar)
         enddo
c
    2    format(I6,' samples read, ',I5,' to',I5//
     :             ' Min, max, mean parameter readings:'/X,34('-'))
    3    format(1X,'par',I3,2X,2(2F7.2,F7.1,'  (',F4.1,')'))
c
       endif
c
       end


