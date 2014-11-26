*+SHOW_CRYO_RYLE

       subroutine show_cryo_ryle (ifile, status)
c      -----------------------------------------
C
C  Displays cryo temperature readings by sample.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only]
C
C  Subroutine to print out the cryo temperature readings recorded with
C  the sample file currently opened on logical unit IFILE, for specified
C  samples.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 15/2/94]
*-
       integer    ifile, status
c
       include '/mrao/post/include/5km_constants.inc'
c
       character  line*36
       integer    itime(3), isecs, iout, iae, isamp, nsamp, length
       integer    samp_status, samp_sid_time, samp_wt, nae
       real*8     samp_ra, samp_dec
c
       integer    value(max_aes)
c
       if (status.ne.0) return
c
       call io_enqout(iout)
       write(iout,*)
c
       nae=max_aes
       do iae=1,nae
         value(iae)=0.0
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
c          write(iout,'(''+'',A/)')line
           do iae=1,nae
             call enq_mon_cryo(ifile,iae,value(iae),status)
           enddo
           write(iout,'(X,A,8F8.2)') line(23:34),(value(iae),iae=1,nae)
         endif
       endif
c
       if (isamp.gt.0) goto 1
c
       end


