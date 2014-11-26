*+CHECK_INTER_CLFST

       subroutine check_inter_clfst (file, plot_device, status)
C      --------------------------------------------------------
C
C  Executes the CHECK-INTERFERENCE command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and reports any occurrences of
C  interference chopping recorded during the observation.  Input samples
C  are treated as interference if the signals for more than half of the
C  spacings are greater than the preset amplitude cut-off.  Rejected input
C  samples are recognised in POSTMORTEM by examining the integration weight
C  which is recorded with each sample (see DUMP-SPACINGS).
C
C  An analysis by spacings can be made by reading the record at the end of
C  the sample file, which contains the total number of samples accepted for
C  each spacing.  Note that this information will not be available for an
C  aborted observation, or one that is still going on.
C
C  The STATUS value should be zero on entry.
C
*-
       character  file*(*), plot_device*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/control_tab_v0.inc'
       include '/mrao/post/include/mon_v1_block.inc'
       include '/mrao/post/include/samp_rt.inc'
c
       character  line*80, list*80
       character  option*5, stime*8, tscope_name*5
       real       pc_reject
       integer    mon_err(12), no_reject, no_error, errors, reject
       integer    ifile, iout, iold, num_samp, isamp, isamp1, isamp2
       integer    itime(3), isecs, i, i1, isp, l, chr_lenb, n, ns
       integer    integ_time, samp_integ
       integer    termi, termo
       integer    block, words
       logical    io_attn, log, table, io_yesno
c
c  Workspace
c
       real       buffer(max_spac)
       integer    ilist(max_spac)
       integer*2  idata(2048)
       common /post/ buffer, ilist, idata
c
c
       if (status.ne.0) return
c
       call io_enqtio(termi,termo)
       call io_enqout(iold)
       iout=iold
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
c
c  Check the telescope type
c
       call enq_tscope( ifile, tscope_name, i, status )
       if (tscope_name.ne.'T151' .and. tscope_name.ne.'38MHZ' ) then
         status = ill_tscope
         return
       end if
c
c  Read the error counts at the end of the run
c
       call enq_numsamp ( ifile, 1, num_samp, status )
       call enq_integ ( ifile, integ_time, samp_integ, status )
c      call read_monitor(ifile,num_samp,mon_length,mon_block,status)
c      if (status.eq.0) then
c        write(iout,*)
c        no_reject=mon_errors(6)/2
c        pc_reject=100.0*float(no_reject)/float(num_samp*samp_integ)
c        if (no_reject.eq.0) then
c          write(iout,'(X,A)')'No interference detected'
c        else
c          write(iout,2)no_reject,pc_reject
c        endif
c      endif
c
       option=' '
       if (status.eq.0) then
c
c  Prompt for further options
c
         if (io_yesno('Do you want to scan the sample file? ','no',
     :                                                     status)) then
           option='SCAN'
           log=io_yesno('Do you want to log the interference? ','no',
     :                                                        status)
c
         elseif (io_yesno('Do you want to plot the sample weights? ',
     :                                                'no',status)) then
           option='PLOT'
c
         elseif (io_yesno('Do you want an analysis by spacing? ',
     :                                                'no',status)) then
           if (istop.eq.0 .or. iabs(ismtyp).ne.1) then
             write(iout,*)'*** interference data not available'
           else
             option='STATS'
             if (io_yesno('tabulate by spacing? ','no',status)) then
               call get_spacings( ifile, 'spacing list :', 'all',
     :                            list, ilist, max_spac, ns, status )
               call chr_chucas(list)
               table=.true.
             endif
           endif
         endif
c
c    Prompt for sample range
c
         if (option.eq.'SCAN' .or. option.eq.'PLOT') then
           isamp1=1
           isamp2=num_samp
           call io_geti('first sample:','*',isamp1,status)
           call io_geti('last sample :','*',isamp2,status)
           isamp1 = max( 1, isamp1 )
           isamp2 = min( num_samp, isamp2 )
         endif
c
c    Prompt for output file
c
         if ((option.eq.'SCAN' .and. log) .or.
     :        option.eq.'STATS') then
           call io_opeout(iout,status)
           if (status.eq.0) then
             if (iout.ne.termo) write(iout,*)'CHECK-INTERFERENCE'
             write(iout,*)
             call io_lstfil(iout,file,status)
           endif
         endif
c
       endif
c
c
c  Scan the sample file reporting samples rejected as interference
c
       if (option.eq.'SCAN' .and. status.eq.0) then
         n=0
         no_error=0
         no_reject=0
         do isamp=isamp1,isamp2
           call read_monitor(ifile,isamp,mon_length,mon_block,status)
           if (status.eq.0) then
c
c      Check integration against sample weight plus correlator errors
c
             errors=mon_errors(1)+mon_errors(2)+mon_errors(3)
     :                           +mon_errors(4)+mon_errors(5)
             reject=samp_integ-samp_wt-(errors-no_error)/2
             if (reject.gt.0) then
               no_reject=no_reject+reject
               isecs=samp_sid_time/10
               call util_stohms(isecs,itime)
               call chr_chtime(itime,stime,l)
               if (log) write(iout,3)isamp,stime,samp_wt,reject
             endif
             n=n+1
           endif
           if (io_attn(status)) goto 1
           no_error=errors
         enddo
c
    1    pc_reject=100.0*float(no_reject)/(n*samp_integ)
c
         write(iout,*)
         write(iout,2)no_reject,pc_reject
         write(iout,4)n,isamp1,isamp1+n-1
c
    2    format(' 2 x',I5,' input samples rejected as interference',
     :                                               6X,'=',F7.2,' %'/)
    3    format(' *** interference,  sample',I5,2X,A,' ST',3X,
     :                                    'weight',I3,',  rejected',I3)
    4    format(I6,' samples read,',I5,' to',I5)
c
c
c  Plot the sample weights
c
       elseif (option.eq.'PLOT' .and. status.eq.0) then
         call plot_wts_clfst(ifile,isamp1,isamp2,plot_device,status)
c
c
c  Print statistics for each spacing using the data stored at the
c  end of the sample file, after the visibility data.
c
       elseif (option.eq.'STATS' .and. status.eq.0) then
         words=lrecb/2
         block=num_samp*(lrecb/1024)+ictab+1
         call io_rdfile(ifile,block,idata,words,status)
         if (status.ne.0) then
           call mon_wrerr(status,'in routine CHECK_INTER_CLFST')
         else
c
c      Print acceptance histogram
c
           do i=1,11
             mon_err(i)=0
           enddo
           do isp=1,nsp
             buffer(isp)=100.*idata(isp)/idata(nsp+1)
             i=ifix(buffer(isp)+0.5)/10
             mon_err(i+1)=mon_err(i+1)+1
           enddo
           write(iout,5)achop*ampscl,aunits(1:chr_lenb(aunits))
           write(iout,6)
           do i=11,1,-1
             i1=10*(i-1)
             if (i.lt.11) write(line,8)mon_err(i),i1,i1+9
             if (i.eq.11) write(line,9)mon_err(i),i1
             l=mon_err(i)/12+1
             line(l+5:65)=' '
             write(iout,*)line
           enddo
           write(iout,*)
c
c    Tabulate by spacing
c
           if (table) then
             do isp=1,nsp
               buffer(isp)=100.0-buffer(isp)
             enddo
             write(iout,7)achop*ampscl,aunits(1:chr_lenb(aunits))
             call table_spac_clfst(ifile,buffer,ilist,ns,1,.false.,
     :                                                        status)
           endif
c
    5      format('0Using the total number of samples accepted for ',
     :     'each spacing,'/' with amplitude cut-off =',F7.1,X,A/)
    6      format(' Number of spacings',40X,'acceptance level')
    7      format('0Samples rejected (%), with amplitude cut-off =',
     :       F7.1,X,A/X,53('-'))
    8      format(I4,':',60('*'),I3,'-',I2,'%')
    9      format(I4,':',60('*'),I6,'%')
c
         endif
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_INTER_CLFST')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


