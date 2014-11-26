

*+CHECK_POINT_RYLE

       subroutine check_point_ryle (file, plot_device, status)
C      -------------------------------------------------------
C
C  Executes the CHECK-POINTING command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and checks aerial pointing in HA and Dec,
C  reporting the mean errors recorded with each data sample.
C
C  The pointing errors can also be logged during a scan through the sample
C  file, errors greater than a chosen threshold being reported for selected
C  aerials.
C
C  It is also possible to plot out the pointing errors within a chosen
C  sample range.  Note that a plot will only be drawn if the mean error
C  is greater than 1.0, or the rms error is greater than 1.0.  The vertical
C  axis of the plot is scaled to run from minimum error to maximum error.
C
C  Alternatively, the actual pointing in HA and Dec for all aerials
C  may be printed out, for any sample.  Note that the HA and Dec values
C  shown include the fixed offsets applied to each aerial.  The pointing
C  errors (actual minus demanded) are also printed out.
C
C  The STATUS value should be zero on entry.
C
C  DJT, 9/3/92
C
*-
       character  file*(*), plot_device*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
c
       character  list*80, option*4
       integer    ilist(max_aes), isamp1, isamp2, nae, num_samp
       integer    ifile, iout, iold, maxc
       integer    termi, termo
       integer    errtype
       logical    log
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
       option=' '

       if (status.eq.0) then
c
c  Prompt for further options
c
         if (io_yesno('Do you want to scan the sample file? ','no',
     :                                                     status)) then
           option='SCAN'
           log=io_yesno('Do you want to log the pointing errors? ',
     :                                                      'no',status)
           if (log) then
             maxc=100
             call io_geti('... log errors greater than:','*',
     :                                                    maxc,status)
           endif
c
         elseif (io_yesno(
     :      'Do you want to plot the mean pointing errors?',
     :                                                'no',status)) then
           option='PLOT'
           errtype=1
         elseif (io_yesno(
     :      'Do you want to plot the mean absolute errors?',
     :                                                'no',status)) then
           option='PLOT'
           errtype=2
         elseif (io_yesno(
     :      'Do you want to show pointing errors by sample?',
     :                                                'no',status)) then
           option='SHOW'
         endif
c
c    Prompt for aerial list and sample range
c
         if (option.eq.'SCAN' .or. option.eq.'PLOT') then
           call get_aerials( ifile, 'aerial list:', 'all', list, ilist,
     :                       max_aes, nae, status                      )
           call enq_numsamp(ifile,1,num_samp,status)
           isamp1=1
           isamp2=num_samp
           call io_geti('first sample:','*',isamp1,status)
           call io_geti('last sample:','*',isamp2,status)
           isamp1=max(1,isamp1)
           isamp2=min(num_samp,isamp2)
         endif
c
c    Prompt for output file
c
         if (option.eq.'SCAN') then
           call io_opeout(iout,status)
           if (status.eq.0) then
             if (iout.ne.termo) write(iout,*)'CHECK-POINTING'
             write(iout,*)
             call io_lstfil(iout,file,status)
             call io_setout(iout)
           endif
         endif
c
       endif
c
c
c  Scan the sample file
c
       if (option.eq.'SCAN') then
         call scan_point_ryle(ifile,list,ilist,nae,isamp1,isamp2,
     :                                                  log,maxc,status)
c
c  Plot pointing for selected aerials
c
       elseif (option.eq.'PLOT') then
         call plot_point_ryle(ifile,list,ilist,nae,isamp1,isamp2,
     :                                       errtype,plot_device,status)
c
c  Print pointing for selected samples
c
       elseif (option.eq.'SHOW') then
         call show_point_ryle(ifile,status)
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_POINT_RYLE')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
