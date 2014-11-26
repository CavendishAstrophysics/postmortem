

*+SHOW_PCS_CLFST

       subroutine show_pcs_clfst (ifile, list, ilist, nae, status)
C      -----------------------------------------------------------
C
C  Displays PC readings by sample.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to print out the PC settings recorded with the sample file
C  currently opened on logical unit IFILE, for aerials included in the
C  given aerial list, and for specified samples.
C
C  The STATUS value should be zero on entry.
C
*-
       character  list*(*)
       integer    ifile, nae, ilist(nae), status
c
       include '/mrao/post/include/global_constants.inc'
c
       character  line*36
       integer    ipc
       integer    itime(3), isecs, iout, iae, isamp,  nsamp, length
       integer    samp_status, samp_sid_time, samp_wt
       real*8     samp_ra, samp_dec
c
       real*4     pc(max_aes)
       common /post/ pc
c
       if (status.ne.0) return
c
       call io_enqout(iout)
       write(iout,*)
c
       do iae=1,max_aes
         pc(iae)=0.0
       enddo
c
       line='sample number : '
       line(31:34)=' ST '
c
c    Prompt for sample number
c
    1  isamp=0
       call enq_numsamp(ifile,1,nsamp,status)
       call io_geti(line(1:16),' ',isamp,status)
       if (isamp.gt.nsamp) then
         write(iout,'(X,A,I5,A)')'*** only',nsamp,' samples present'
c
       elseif (isamp.gt.0) then
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                          samp_sid_time,samp_wt,status)
         if (status.eq.0) then
           isecs=samp_sid_time/10
           call util_stohms(isecs,itime)
           call chr_chtime(itime,line(23:30),length)
           call chr_chitoc(isamp,line(17:20),length)
           call chr_chljus(line(17:20),length)
           write(iout,'(''+'',A/)')line
           do iae=1,nae
             call enq_mon_pc(ifile,ilist(iae),ipc,status)
             pc(iae)=ipc
           enddo
           call table_aes(ifile,pc,ilist,nae,-1,status)
         endif
       endif
c
       if (isamp.gt.0) goto 1
c
       end
