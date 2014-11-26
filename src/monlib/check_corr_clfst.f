*+CHECK_CORR_CLFST

       subroutine check_corr_clfst (file, status)
C      ------------------------------------------
C
C  Executes the CHECK-CORRELATOR command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Scans the current sample file (or part of it) and reports the recorded
C  correlator error counts, and any occurrences of missed samples.  Errors
C  are recognised during an observation by checking the totally correlated
C  count, which should be present in channel 125 of the input buffer and
C  should correspond to the preset correlator integration time.  Timing
C  errors, causing samples to be missed, reflect imperfect synchronisation
C  between the real-time programs.  Note that if a correlator error is
C  seen, then both of a pair of input samples are rejected.  Six classes
C  of error are recognised:
C
C         - sample set to 000000, all bits cleared
C         - sample buffer misaligned, 'shift' error
C         - error in correlator integration time, total count wrong
C         - sample set to 177777, all bits set
C         - timing error, samples missed.
C         - corrected shift errors.
C
C  Checks are also made of the hut status words.  For more details about any
C  reported local oscillator problems, use  CHECK-LOCAL-OSC  in MICRO-LOG.
C
C  The STATUS value should be zero on entry.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v1_block.inc'
       include '/mrao/post/include/samp_rt.inc'
c
       character  option*4, stime*8, tscope_name*5
       real       pc_errors, pc_reject
       integer    no_errors, no_reject, no_shift, no_missing
       integer    ifile, iout, iold, nsamp, isamp, isamp1, isamp2
       integer    nhut_bad, errors, reject, count
       integer    itime(3), isecs, i, length, n
       integer    integ_time, samp_integ
       logical    log_err, log_shift
c
c
       if (status.ne.0) return
c
       call io_enqout(iold)
       iout=iold
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
c
c check telescope type
c
       call enq_tscope( ifile, tscope_name, i, status )
       if (tscope_name.ne.'T151' .and. tscope_name.ne.'38MHZ' ) then
         status = ill_tscope
         return
       end if
c
c  Check hut status
c
       call check_huts_clfst(nhut_bad,status)
c
c  Read the error counts at the end of the run
c
       call enq_numsamp(ifile,1,nsamp,status)
       call enq_integ(ifile,integ_time,samp_integ,status)
       call read_monitor(ifile,nsamp,mon_length,mon_block,status)
       if (status.eq.0) then
         no_errors=(mon_errors(1)+mon_errors(2)+mon_errors(3)
     :                           +mon_errors(4)+mon_errors(5))/2
         no_shift=mon_errors(6)
         pc_errors=100.0*float(no_errors)/float(nsamp*samp_integ)
         if (no_errors+no_shift.eq.0) then
           write(iout,'(/X,A)')'No correlator errors'
         else
           write(iout,2)(mon_errors(i)/2,i=1,5),mon_errors(6)
           write(iout,3)no_errors,pc_errors
         endif
c
       endif
c
       option=' '
       write(iout,*)
       if (no_errors+no_shift.gt.0 .and. status.eq.0) then
c
c  Prompt for further options
c
         if (io_yesno('Do you want to scan the sample file? ','no',
     :                                                     status)) then
           option='SCAN'
           log_err=io_yesno('Do you want to log correlator errors? ',
     :                                                   'no', status)
           log_shift=io_yesno('Do you want to log shift errors? ',
     :                                                   'no', status)
           isamp1=1
           isamp2=nsamp
           call io_geti('first sample:','*',isamp1,status)
           call io_geti('last sample:','*',isamp2,status)
           isamp1=max(1,isamp1)
           isamp2=min(nsamp,isamp2)
         endif
c
c    Prompt for output file
c
         if (option.eq.'SCAN') then
           call io_opeout( iout, status )
           if (status.eq.0) then
             if (iout.gt.1) write(iout,*)'CHECK-CORRELATOR'
             write(iout,*)
             call io_lstfil(iout,file,status)
           endif
         endif
c
       endif
c
c
c  Scan the sample file reporting correlator errors
c
       if (option.eq.'SCAN' .and. status.eq.0) then
         n=0
         count=0
         no_errors=0
         no_reject=0
         no_shift=0
         no_missing=0
         do isamp=isamp1,isamp2
           call read_monitor(ifile,isamp,mon_length,mon_block,status)
           if (status.eq.0) then
             isecs=samp_sid_time/10
c
c      Check for missing samples
c
             if (mon_errors(5).gt.no_missing) then
               call util_stohms(isecs,itime)
               call chr_chtime(itime,stime,length)
               if (log_err) write(iout,5)'    timing error',isamp,
     :                      stime,(mon_errors(i)/2,i=1,5),mon_errors(6)
             endif
c
c      Check for correlator errors
c
             errors=mon_errors(1)+mon_errors(2)+mon_errors(3)
     :                                         +mon_errors(4)
             if (errors.gt.no_errors) then
               call util_stohms(isecs,itime)
               call chr_chtime(itime,stime,length)
               if (log_err) write(iout,5)'correlator error',isamp,
     :                      stime,(mon_errors(i)/2,i=1,5),mon_errors(6)
             endif
c
c      Check for corrected shift errors
c
             if (mon_errors(6).gt.no_shift) then
               call util_stohms(isecs,itime)
               call chr_chtime(itime,stime,length)
               if (log_shift)write(iout,5)' corrected shift',isamp,
     :                      stime,(mon_errors(i)/2,i=1,5),mon_errors(6)
             endif
c
c      Check for interference
c
             reject=samp_integ-samp_wt-(errors-no_errors)/2-
     :                             (mon_errors(5)-no_missing)/2
             if (reject.gt.0) then
               no_reject=no_reject+reject
             endif
             n=n+1
           endif
           if (isamp.eq.isamp1) count=errors+mon_errors(5)
           if (io_attn(status)) goto 1
           no_missing=mon_errors(5)
           no_shift=mon_errors(6)
           no_errors=errors
         enddo
c
    1    no_errors=(no_errors+no_missing-count)/2
         pc_errors=100.0*float(no_errors)/(n*samp_integ)
         pc_reject=100.0*float(no_reject)/(n*samp_integ)
         if (log_err) write(iout,*)
         write(iout,3)no_errors,pc_errors
         write(iout,4)no_reject,pc_reject
         write(iout,6)n,isamp1,isamp1+n-1
c
    2    format(/' Correlator errors:'//
     :     I9,' - samples set to 000000, all bits cleared'/
     :     I9,' - sample buffer misaligned, ''shift'' errors'/
     :     I9,' - errors in integration time, total count wrong'/
     :     I9,' - samples set to 177777, all bits set'/
     :     I9,' - timing errors, samples missed'/
     :     I9,' - corrected shift errors'/)
c
    3    format(' 2 x',I5,' input samples lost through correlator',
     :   ' error =',F7.2,' %')
    4    format(' 2 x',I5,' input samples rejected as interference',
     :       6X,'=',F7.2,' %')
    5    format(' *** ',A,',  sample',I5,2X,A,' ST',6I5)
    6    format(/I6,' samples read,',I5,' to',I5)
c
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_CORR_CLFST')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


