

*+SCAN_PCS_CLFST

       subroutine scan_pcs_clfst (ifile, list, ilist, nae,
     :                                    isamp1, isamp2, maxc, status)
C      ----------------------------------------------------------------
C
C  Scans PC values within sample file.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      MAXC          integer       log PC changes > MAXC
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to scan the sample file currently opened on logical unit IFILE,
C  and report changes in the PC settings as recorded during the observation,
C  for the aerials included in the given aerial list.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, modified by PA for use with V1 and V2 CT, 9/11/88]
*-
       character  list*(*)
       integer    ifile, nae, ilist(nae)
       integer    isamp1, isamp2, status
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c
       character  stime*12
       integer    i, iae, isamp, length
       integer    itime(3), isecs, iout, maxc, n
       logical    report, test
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
c
c  Workspace
c
       integer    ipc(max_aes), pc(max_aes)
       common /post/ pc, ipc
c
c
       if (status.ne.0) return
c
       stime(9:12)=' ST '
       call io_enqout(iout)
c
       do iae=1,max_aes
         ipc(iae)=0
       enddo
c
       if (nae.le.10) then
         write(iout,'(/23X,A,10I5)')'aerials',(ilist(i),i=1,nae)
         write(iout,*)
       else
         write(iout,'(/33X,A,A/)')'aerials : ',list(1:chr_lenb(list))
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
c  For each aerial in the list, report PC changes greater than MAXC
c
           do i=1,nae
             iae=ilist(i)
             call enq_aestat(ifile, iae, 1, test, status)
             if (test) then
               call enq_mon_pc(ifile, iae, pc(iae), status)
               if (iabs(ipc(iae)-pc(iae)).gt.maxc) report=.true.
             endif
           enddo
           n=n+1
c
           if (report) then
             do iae=1,max_aes
               ipc(iae)=pc(iae)
             enddo
             isecs=samp_sid_time/10
             call util_stohms(isecs,itime)
             call chr_chtime(itime,stime(1:8),length)
             write(iout,1)isamp,stime,(ipc(ilist(i)),i=1,nae)
    1        format(' sample',I5,2X,A,4X,10I5/(30X,10I5))
             if (io_attn(status)) return
           endif
c
         endif
       enddo
c
       end
