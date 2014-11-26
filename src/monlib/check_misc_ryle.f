*+CHECK_MISC_RYLE

       subroutine check_misc_ryle (file, plot_device, status)
C      ------------------------------------------------------
C
C  Executes the CHECK-PARAMETERS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and reports miscellaneous parameter
C  readings recorded during the observation.  A table is produced of
C  minimum, maximum and mean values (and rms deviations) during the run.
C
C  Changes in the readings during the run may also be logged during a scan
C  through the sample file.  A maximum reported change can be set, to monitor
C  large fluctuations (samples will be reported only when the readings have
C  altered by the preset level).  Set this level to -1 to display all samples.
C
C  The readings may also be plotted out, within a selected sample range.
C
C  Alternatively, the complete set of readings for all aerials can be
C  displayed for any chosen sample.
C
C  The STATUS value should be zero on entry.
C (11 Aug 03: GP)
*-
       character  file*(*), plot_device*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/global_constants.inc'
c
       integer    max_pars
       parameter (max_pars=3*max_rt_aes)
       integer    ilist(max_pars), isamp1, isamp2, npar, num_samp
       integer    ifile, iout, iold, termi, termo
       character  list*24, option*4
       real       rmax
       logical    log, io_yesno
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
           log=.false.
           option='SCAN'
           if (io_yesno('Do you want to log fluctuations? ','no',
     :                                                     status)) then
             rmax=1.0
             log=.true.
             call io_getr('... report changes greater than:','*',
     :                                                    rmax,status)
           endif
c
         elseif (io_yesno('Do you want to plot parameter readings? ',
     :                                               'yes',status)) then
           option='PLOT'
         elseif (io_yesno('Do you want to display readings by sample?',
     :                                                'no',status)) then
           option='SHOW'
         endif
c
c    Prompt for parameter list and sample range
c
         npar=0
         if (option.ne.' ') then
           call io_getlst( 'parameter list:', ' ', list, ilist,
     :                                           max_pars, npar, status)
         endif
         if (option.eq.'SCAN' .or. option.eq.'PLOT') then
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
           call io_opeout( iout, status )
           if (status.eq.0) then
             if (iout.ne.termo) write(iout,*)'CHECK-PARAMETERS'
             write(iout,*)
             call io_lstfil(iout,file,status)
           endif
         endif
c
       endif
c
c
c  Scan the sample file
c
       if (option.eq.'SCAN') then
         call scan_misc_ryle(ifile,ilist,npar,isamp1,isamp2,
     :                                                  log,rmax,status)
c
c  Plot parameter readings
c
       elseif (option.eq.'PLOT') then
         call plot_misc_ryle(ifile,ilist,npar,isamp1,isamp2,
     :                                           plot_device,status)
c
c  Print parameter readings for selected samples
c
       elseif (option.eq.'SHOW') then
         call show_misc_ryle(ifile,ilist,npar,status)
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_MISC_RYLE')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


