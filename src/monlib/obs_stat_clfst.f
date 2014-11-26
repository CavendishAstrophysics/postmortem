
*+OBS_STAT_CLFST

       subroutine obs_stat_clfst (status)
C      ------------------------------------
C
C  Executes the OBSERVING-STATUS command.
C
C  Given:
C      STATUS        integer       status value
C
C  Routine to print out the current LSFT observing status to the
C  current output device.
C
C  The STATUS values should be zero on entry.
C
*-
       integer    status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
       include '/mrao/post/include/mon_v1_block.inc'
       include '/mrao/include/constants.inc'
c
       character  string*60, tables*40
       character  date*24, chra*16, chdec*16, ltime*3
       integer    iae, iae1, iae2, ihut, nae_bad, nhut_bad
       integer    idate(3), ifile, iout, isam, l, ld, lr, chr_lenb
       integer    no_errors, no_reject, obs_flag, prsec
       real       pc_errors, pc_reject
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
c  Find the current sample file from observing tables
c
       tables='(TELESCOPE)CONTROL-TABLES:DATA'
       call io_opefil(ifile,tables,'rx',0,status)
       call read_ct(ifile,status)
       close(ifile)
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,sfile,'read',0,status)
       call open_source(ifile,1,0,status)
c
c  Print current observing status
c
       ltime='GMT'
       if (iutim.eq.1) ltime='BST'
       call enq_obsstat(obs_flag)
       if (obs_flag.eq.0) then
         write(iout,*)'No observation in progress'
       elseif (obs_flag.eq.1) then
         write(iout,*)'... observation queued'
       elseif (obs_flag.eq.2) then
         write(iout,*)'... observation in progress'
       elseif (obs_flag.eq.3) then
         write(iout,*)'... correlator test running'
       endif
c
       if (obs_flag.eq.0 .or. obs_flag.eq.3) then
         idate(1)=idat1(1)
         idate(2)=idat1(2)
         idate(3)=idat1(3)
         call chr_chdate(idate,3,date,ld)
         write(iout,*)'Last observation was ',date(1:ld)
       endif
c
c  Print observing frequency
c
       write(iout,'(/A,F6.1,A)')' Observing frequency',frobs/1.e6,' MHZ'
       if (ramc.lt.0.) write(iout,*)'non-tracking observation'
       write(iout,*)
c
c  Print observation title
c
       write(iout,'(2A)')' Title : ',title
       l=chr_lenb(ctext(1:80))
       if (l.gt.0) write(iout,*)ctext(1:l)
c
c  Print map centre (phase centre)
c
       prsec=1
       if (ramc.ge.0) then
         call chr_chdtos(raref/const_h2r,prsec,chra,lr)
         call chr_chdtos(decref/const_d2r,prsec,chdec,ld)
         write(iout,'(X,A,2X,A,X,A)')
     :     'Map centre (1950)',chra(1:lr),chdec(1:ld)
         call chr_chdtos(ramc/const_h2r,prsec,chra,lr)
         call chr_chdtos(decmc/const_d2r,prsec,chdec,ld)
         write(iout,'(X,A,X,A,X,A)')
     :    'precessed position',chra(1:lr),chdec(1:ld)
       else
         call chr_chdtos(raref/const_h2r,prsec,chra,lr)
         call chr_chdtos(decref/const_d2r,prsec,chdec,ld)
         write(iout,'(X,A,9X,A,X,A)')
     :            'Map centre',chra(1:lr),chdec(1:ld)
       endif
c
c  Print observation start and stop times
c
c      isam=2*intsam*isamps/10
       isam=2*intsam*(itab(1)+itab(2))/10000
       write(iout,1) nsp,isam
       write(iout,2) 'start time ',
     :   istim1(3),istim1(2),istim1(1),itim1(3),itim1(2),ltime
       write(iout,2) 'stop time .',
     :   istim2(3),istim2(2),istim2(1),itim2(3),itim2(2),ltime
    1  format(I4,' spacings ',7('.'),' integrating by',I4,' secs')
    2  format(X,A,9('.'),X,2(I2.2,'.'),I2.2,' ST',3X,I2.2,'.',I2.2,X,A)
c
c
c  If no current observation
c
       if (obs_flag.eq.0 .or. obs_flag.eq.3) then
         write(iout,*)
c
c    Report run aborted
c
         if (istop.eq.0 .or. nsamp.eq.0) then
           write(iout,*)'... run was aborted'
         else
c
c    Report hut or aerial problems
c
           l=20
           nhut_bad=0
           string='*** problem with hut'
           do ihut=1,max_huts
             if (btest(ihutstat(ihut),15).ne.0) then
               if (btest(ihutstat(ihut),12).ne.0. or.
     :             btest(ihutstat(ihut),10).ne.0) then
                 string(l+2:l+2)=char(ihut+64)
                 nhut_bad=nhut_bad+1
                 l=l+2
               endif
             endif
           enddo
           if (nhut_bad.gt.0) write(iout,*)string(1:l)
c
           l=23
           nae_bad=0
           string='*** problem with aerial'
           do ihut=1,max_huts
             call enq_ae_hut(ifile,ihut,iae1,iae2,status)
             if (btest(ihutstat(ihut),15).ne.0 .and.
     :           btest(ihutstat(ihut),13).ne.0)then
c
               do iae=iae1,iae2
                 if (btest(iaestat(iae),12).ne.0 .or.
     :               btest(iaestat(iae),10).ne.0) then
                   if (l+3.gt.60) then
                     write(iout,*)string(1:l)
                     l=0
                   endif
                   write(string(l+1:l+3),'(I3)')iae
                   nae_bad=nae_bad+1
                   l=l+3
                 endif
               enddo
             endif
           enddo
           if (nae_bad.gt.0) write(iout,*)string(1:l)
c
c
c    Report correlator and interference problems
c
           call read_monitor(ifile,nsamp,mon_length,mon_block,status)
           if (status.eq.0) then
             no_errors=(mon_errors(1)+mon_errors(2)+mon_errors(3)
     :                               +mon_errors(4)+mon_errors(5))/2
             no_reject=mon_errors(6)/2
             pc_errors=100.0*float(no_errors)/float(intsam*nsamp)
             pc_reject=100.0*float(no_reject)/float(intsam*nsamp)
             if (pc_errors.ge.1.0) write(iout,3) nint(pc_errors)
             if (pc_reject.ge.1.0) write(iout,4) nint(pc_reject)
    3        format(X,I2,'% of samples lost through correlator error')
    4        format(X,I2,'% of samples rejected as interference')
           endif
c
         endif
       endif
c
       if (status.ne.0) then
         call mon_wrerr(status,'in routine OBS_STAT_CLFST')
         status=0
       endif
c
       call close_sf(ifile,status)
c
       write(iout,*)
c
       end
