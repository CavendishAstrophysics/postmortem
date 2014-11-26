*+SCAN_WIND_GAUGE

       subroutine scan_wind_gauge (ifile, isamp1, isamp2, status)
C      ----------------------------------------------------------
C
C  Scans wind gauge readings within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only].
C
C  Subroutine to scan the sample file currently opened on logical unit
C  IFILE, and report the wind gauge readings recorded during the observation,
C  for the aerials included in the given aerial list.  A table is produced
C  of the minimum, maximum and mean readings within the specified sample
C  range.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 19/1/93]
*-
       integer    ifile, isamp1, isamp2, status
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       integer    isamp, n
       integer    iout
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
c
       real*8     wind_mean, wind_rms
       real*4     wind_min, wind_max
       real*4     value
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       wind_min=100.0
       wind_max=-100.0
       wind_mean=0.d0
       wind_rms=0.d0
c
       n=0
       do isamp=isamp1,isamp2
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                          samp_sid_time,samp_wt,status)
         call enq_mon_wind(ifile,value,status )
c
c    Accumulate min, max, sum, sum**2
c
         if (value.lt.wind_min) then
           wind_min=value
         elseif (value.gt.wind_max) then
           wind_max=value
         endif
         wind_mean=wind_mean+value
         wind_rms=wind_rms+value*value
         n=n+1
c
       enddo
c
c    Print out min, max, mean, rms
c
       if (n.gt.0) then
         wind_mean=wind_mean/n
         wind_rms=wind_rms/n-wind_mean*wind_mean
         wind_rms=dsqrt(dmax1(0.d0,wind_rms))
         write(iout,2)n,isamp1,isamp1+n-1
         write(iout,3)wind_min,wind_max,wind_mean,wind_rms
c
    2    format(I6,' samples read, ',I5,' to',I5/)
    3    format(' Min, max, mean windspeed (knots):',
     :                       2X,2(2F7.2,F7.1,'  (',F4.1,')'))
c
       endif
c
       end


