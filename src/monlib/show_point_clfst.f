

*+SHOW_POINT_CLFST

       subroutine show_point_clfst (ifile, list, ilist, nae, option,
     :                                                          status)
C      ----------------------------------------------------------------
C
C  Displays aerial pointing by sample.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      OPTION        char*4        'HA','DEC' or 'BOTH'
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to display HA/Dec pointing together with pointing errors,
C  from the sample file currently opened on logical unit IFILE.  The
C  values recorded with the sample file are printed out for selected
C  aerials, and for specified samples.
C
C  The STATUS value should be zero on entry.
C
*-
       character  list*(*), option*4
       integer    ifile, nae, ilist(nae), status
c
       include '/mrao/post/include/global_constants.inc'
c
       character  line*36
       integer    itime(3), isecs, iout, length
       integer    samp_status, samp_sid_time, samp_wt
       integer    i, iae, isamp, nsamp
       real*8     samp_ra, samp_dec
c
c  Workspace
c
       real*4     values(max_aes)
       real*4     hoffset(max_aes), doffset(max_aes)
       integer    iha(2,max_aes), idec(2,max_aes)
c
       common /post/ values, hoffset, doffset, iha, idec
c
c
       if (status.ne.0) return
c
       call io_enqout(iout)
       write(iout,*)
c
       do iae=1,max_aes
         values(iae)=0.0
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
c
c    Print out HA pointing and errors
c
           if (option.eq.'HA' .or. option.eq.'BOTH') THEN
             do i=1,nae
               iae=ilist(i)
               call enq_mon_hadec(ifile,iae,iha(1,iae),idec(1,iae),
     *                                  status)
               call enq_offsets(ifile,iae,hoffset(iae),doffset(iae),
     *                                  status)
               values(i)=iha(2,iae)-hoffset(iae)
             enddo
             write(iout,'(X,A/)')'HA pointing'
             call table_aes(ifile,values,ilist,nae,-1,status)
             do i=1,nae
               iae=ilist(i)
               values(i)=iha(2,iae)-iha(1,iae)
             enddo
             write(iout,'(X,A/)')'... HA pointing errors'
             call table_aes(ifile,values,ilist,nae,-1,status)
           endif
c
c    Print out Dec pointing and errors
c
           if (option.eq.'DEC' .or. option.eq.'BOTH') then
             do i=1,nae
               iae=ilist(i)
               call enq_mon_hadec(ifile,iae,iha(1,iae),idec(1,iae),
     *                                  status)
               call enq_offsets(ifile,iae,hoffset(iae),doffset(iae),
     *                                  status)
               values(i)=idec(2,iae)-doffset(iae)
             enddo
             write(iout,'(X,A/)')'Dec pointing'
             call table_aes(ifile,values,ilist,nae,-1,status)
             do i=1,nae
               iae=ilist(i)
               values(i)=idec(2,iae)-idec(1,iae)
             enddo
             write(iout,'(X,A/)')'... Dec pointing errors'
             call table_aes(ifile,values,ilist,nae,-1,status)
           endif
c
         endif
       endif
c
       if (isamp.gt.0) goto 1
c
       end
