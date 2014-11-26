

*+SHOW_POINT_RYLE

       subroutine show_point_ryle (ifile, status)
C      ------------------------------------------
C
C  Displays aerial pointing by sample.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to display pointing errors, from the sample file currently
C  opened on logical unit IFILE.  The values recorded with the sample file
C  are printed out for selected samples.
C
C  The STATUS value should be zero on entry.
C
*-
       integer    ifile, status
c
       include '/mrao/post/include/5km_constants.inc'
c
       character  line*36
       integer    itime(3), isecs, iout, length
       integer    samp_status, samp_sid_time, samp_wt
       integer    iae, isamp, nsamp, nae
       real*8     samp_ra, samp_dec
c
c  Workspace
c
       real*4     values(max_aes)
       integer    iha(2,max_aes), idec(2,max_aes)
c
       common /post/ values, iha, idec
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       nae = max_aes
       do iae=1,nae
         values(iae)=0.0
       enddo
c
       line='sample number : '
       line(31:34)=' ST '
c
c    Prompt for sample number
c
    1  isamp=0
       write(iout,*)
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
           write(iout,'(/X,A)')line(23:34)
c          write(iout,'(''+'',A/)')line
c
c    Print out HA pointing errors
c
           write(iout,*)
c          write(iout,'(X,A/)')'HA pointing errors'
           do iae=1,nae
             call enq_mon_hadec(ifile,iae,iha(1,iae),idec(1,iae),status)
             values(iae)=iha(1,iae)
           enddo
           write(iout,10) 'HA mean     ',(values(iae),iae=1,nae)
           do iae=1,nae
             values(iae)=iha(2,iae)
           enddo
           write(iout,10) 'HA mean abs ',(values(iae),iae=1,nae)
c
c    Print out Dec pointing errors
c
           write(iout,*)
c          write(iout,'(X,A/)')'Dec pointing errors'
           do iae=1,nae
             call enq_mon_hadec(ifile,iae,iha(1,iae),idec(1,iae),status)
             values(iae)=idec(1,iae)
           enddo
           write(iout,10) 'Dec mean    ',(values(iae),iae=1,nae)
           do iae=1,nae
             values(iae)=idec(2,iae)
           enddo
           write(iout,10) 'Dec mean abs',(values(iae),iae=1,nae)
c
   10      format (1X,A,8F8.2)
c
         endif
       endif
c
       if (isamp.gt.0) goto 1
c
       end
