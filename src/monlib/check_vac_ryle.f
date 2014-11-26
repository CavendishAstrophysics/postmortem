*+CHECK_VAC_RYLE

       subroutine check_vac_ryle (file, plot_device, status)
C      -----------------------------------------------------
C
C  Executes the CHECK-VACUUM command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and reports the vacuum readings
C  recorded during the observation, for selected aerials.  A table is
C  produced of minimum, maximum and mean readings (and rms deviations)
C  during the run.
C
C  Changes in the readings during the run may also be logged during a scan
C  through the sample file.  A maximum reported change can be set, to monitor
C  large fluctuations (samples will be reported only when the readings have
C  altered by the preset level).  Set this level to -1 to display all samples.
C
C  The readings may also be plotted out, within a selected sample range.
C  Note that they are displayed for all aerials on axes with a range 0-1v.
C
C  Alternatively, the complete set of cryo readings for all aerials can be
C  printed out for any chosen sample.
C
C  The STATUS value should be zero on entry.
C (GP 11 Aug 03)
*-
       character  file*(*), plot_device*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
c
       character  list*80, option*4
       integer    ilist(max_aes), nae, num_samp
       integer    ifile, iout, iold, termi, termo
       integer    isamp, isamp1, isamp2
       real       max_value
       real       rmax
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
           log=.false.
           option='SCAN'
           if (io_yesno('Do you want to log changes? ','no',
     :                                                     status)) then
             rmax=10.0
             log=.true.
             call io_getr('... report changes greater than:','10.0',
     :                                                     rmax,status)
           endif
c
         elseif (io_yesno('Do you want to plot readings? ','yes',
     :                                                     status)) then
           option='PLOT'
         elseif (io_yesno('Do you want to display readings by sample?',
     :                                                'no',status)) then
           option='SHOW'
         endif
c
c    Prompt for aerial list and sample range
c
         if (option.eq.'PLOT') then
           isamp=1
           max_value=100.0
c          call set_monitor(ifile,isamp,status)
c          do i=1,nae
c            call enq_mon_vac(ifile,i,value,status)
c            if (value.gt.1.0) max_value=10.0
c          enddo
c          call io_getr('maximum value:','*',max_value,status)
           call get_aerials( ifile, 'aerial list:', 'all', list, ilist,
     :                       max_aes, nae, status                      )
           call chr_chucas(list)
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
             if (iout.ne.termo) write(iout,*)'CHECK-VACUUM'
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
         call scan_vac_ryle(ifile,isamp1,isamp2,log,rmax,status)
c
c  Plot readings for selected aerials
c
       elseif (option.eq.'PLOT') then
         call plot_vac_ryle(ifile,list,ilist,nae,isamp1,isamp2,
     :                                     max_value,plot_device,status)
c
c  Print readings for selected samples
c
       elseif (option.eq.'SHOW') then
         call show_vac_ryle(ifile,status)
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_VAC_RYLE')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


