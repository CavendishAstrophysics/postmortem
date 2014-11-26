

*+SCAN_POINT_CLFST

       subroutine scan_point_clfst (ifile, list, ilist, nae,
     :                                isamp1, isamp2, log, maxc, status)
C      -----------------------------------------------------------------
C
C  Scans aerial pointing data within the sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      LOG           logical       log pointing errors control flag
C      MAXC          integer       log pointing errors > MAXC
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to scan the sample file currently opened on logical unit IFILE,
C  and report the HA and Dec pointing data recorded during the observation,
C  for the aerials included in the given aerial list.  A table is produced
C  of the minimum, maximum and mean pointing errors within the given
C  sample range.  Pointing errors may also be logged during the scan.
C
C  The STATUS value should be zero on entry.
C
*-
       character  list*(*)
       integer    ifile, nae, ilist(nae)
       integer    isamp1, isamp2, status
       logical    log
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/mon_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  line*72, stime*12
       real*8     samp_ra, samp_dec
       integer    ha_update, dec_update
       integer    ha_err, dec_err, mh
       integer    i, iae, iae1, iae2, ihut, isamp, length
       integer    itime(3), isecs, iout, maxc, n
       integer    samp_status, samp_sid_time, samp_wt
       integer    mon_update(4)
       integer    termi, termo
       logical    test
c
c  Workspace
c
       real*8     ha_mean(max_aes), ha_rms(max_aes)
       real*8     dec_mean(max_aes), dec_rms(max_aes)
       real*4     hoffset(max_aes), doffset(max_aes)
       integer    ha_min(max_aes), ha_max(max_aes)
       integer    dec_min(max_aes), dec_max(max_aes)
       integer    iha(2,max_aes), idec(2,max_aes)
c
       common /post/ ha_mean, ha_rms, dec_mean, dec_rms,
     :               ha_min, ha_max, dec_min, dec_max,
     :               hoffset, doffset, iha, idec
c
c
       if (status.ne.0) return
c
       call io_enqtio(termi,termo)
       call io_enqout(iout)
       stime(9:12)=' ST '
       if (iout.ne.termo) then
         write(iout,'(X,A,A/)')'Aerials : ',list(1:chr_lenb(list))
       endif
c
       do iae=1,max_aes
         ha_min(iae)=max_par
         ha_max(iae)=-max_par
         dec_min(iae)=max_par
         dec_max(iae)=-max_par
         ha_mean(iae)=0.d0
         ha_rms(iae)=0.d0
         dec_mean(iae)=0.d0
         dec_rms(iae)=0.d0
       enddo
c
       n=0
       ha_update=0
       dec_update=0
       do isamp=isamp1,isamp2
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                          samp_sid_time,samp_wt,status)
         call enq_mon_update(ifile,mon_update,status)
         if (status.eq.0) then
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,stime(1:8),length)
c
c  For each aerial in the list
c
           do i=1,nae
             iae=ilist(i)
             call enq_aestat( ifile, iae, 1, test, status )
             if (test) then
c
               call enq_mon_hadec(ifile, iae,
     :                           iha(1,iae),idec(1,iae),status)
               call enq_offsets(ifile, iae,
     :                           hoffset(iae),doffset(iae),status)
               ha_err=iha(2,iae)-iha(1,iae)
               dec_err=idec(2,iae)-idec(1,iae)
c
c    Report pointing errors greater than MAXC
c
               if (log) then
                 if (mon_update(1).gt.ha_update) then
                   if (iabs(ha_err).gt.maxc) then
                     iha(1,iae)=iha(1,iae)-hoffset(iae)
                     iha(2,iae)=iha(2,iae)-hoffset(iae)
                     write(iout,1)isamp,stime,iae,'HA ',
     :                                  iha(1,iae),iha(2,iae)
                   endif
                 endif
                 if (mon_update(2).gt.dec_update) then
                   if (iabs(dec_err).gt.maxc) then
                     idec(1,iae)=idec(1,iae)-doffset(iae)
                     idec(2,iae)=idec(2,iae)-doffset(iae)
                     write(iout,1)isamp,stime,iae,'Dec',
     :                                  idec(1,iae),idec(2,iae)
                   endif
                 endif
               endif
c
    1          format(' sample',I5,2X,A,5X,'ae',I3,3X,A,
     :                                ' requested',I5,2X,'actual',I5)
c
c    Accumulate min, max, sum, sum**2 pointing errors
c
               if (ha_err.lt.ha_min(iae)) then
                 ha_min(iae)=ha_err
               elseif (ha_err.gt.ha_max(iae)) then
                 ha_max(iae)=ha_err
               endif
               if (dec_err.lt.dec_min(iae)) then
                 dec_min(iae)=dec_err
               elseif (dec_err.gt.dec_max(iae)) then
                 dec_max(iae)=dec_err
               endif
               ha_mean(iae)=ha_mean(iae)+ha_err
               dec_mean(iae)=dec_mean(iae)+dec_err
               ha_rms(iae)=ha_rms(iae)+ha_err*ha_err
               dec_rms(iae)=dec_rms(iae)+dec_err*dec_err
             endif
           enddo
c
           ha_update=mon_update(1)
           dec_update=mon_update(2)
           n=n+1
c
           if (io_attn(status)) return
c
         endif
       enddo
c
       if (log) write(iout,*)
       write(iout,2)n,isamp1,isamp1+n-1
c
c    Print out min, max, mean, rms by hut
c
       if (n.gt.0 .and..not.log) then
c
         write(iout,3)
c
         do i=1,nae
           iae=ilist(i)
           ha_mean(iae)=ha_mean(iae)/n
           dec_mean(iae)=dec_mean(iae)/n
           ha_rms(iae)=ha_rms(iae)/n-ha_mean(iae)*ha_mean(iae)
           ha_rms(iae)=dsqrt(dmax1(0.d0,ha_rms(iae)))
           dec_rms(iae)=dec_rms(iae)/n-dec_mean(iae)*dec_mean(iae)
           dec_rms(iae)=dsqrt(dmax1(0.d0,dec_rms(iae)))
         enddo
c
         call enq_groups(ifile,mh,status)
         do ihut=1,mh
           n=0
           call enq_ae_hut(ifile,ihut,iae1,iae2,status)
           do iae=iae1,iae2
             do i=1,nae
               if (iae.eq.ilist(i)) then
                 n=n+1
                 call enq_aestat( ifile, iae, 1, test, status )
                 if (test) then
                   write(line,4)iae,
     :              ha_min(iae),ha_max(iae),ha_mean(iae),ha_rms(iae),
     :              dec_min(iae),dec_max(iae),dec_mean(iae),dec_rms(iae)
                 else
                   write(line,5)iae
                 endif
                 if (n.eq.1) write(iout,*)
                 write(iout,*)line
               endif
             enddo
           enddo
           if (io_attn(status)) return
         enddo
c
    2    format(I5,' samples read,',I5,' to',I5)
    3    format('0Min, max, mean pointing errors in HA and Dec:'/
     :           X,44('-'))
    4    format(2X,'ae',I3,2X,2(2I7,F7.1,'  (',F4.1,')'))
    5    format(2X,'ae',I3,2X,8('     . '))
c
       endif
c
       end
