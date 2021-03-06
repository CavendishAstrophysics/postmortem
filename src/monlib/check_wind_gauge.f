*+CHECK_WIND_GAUGE

       subroutine check_wind_gauge (file, plot_device, status)
C      -------------------------------------------------------
C
C  Executes the CHECK-WIND-GAUGE command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and reports wind gauge readings
C  recorded during the observation.   A table is produced of minimum,
C  maximum and mean windspeed (and rms deviation) during the run.
C
C  The readings may also be plotted, within a selected sample range.
C
C  The STATUS value should be zero on entry.
C last mod:  GP  1 Aug 03
*-
       character  file*(*), plot_device*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
c
       character  option*4
       integer    isamp1, isamp2, num_samp
       integer    ifile, iout, iold
       integer    termi, termo
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
c
         elseif(io_yesno('Do you want to plot the windspeed readings? ',
     :                                               'yes',status)) then
           option='PLOT'
         endif
c
c    Prompt for sample range
c
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
             if (iout.ne.termo) write(iout,*)'CHECK-WIND-GAUGE'
             write(iout,*)
             call io_lstfil(iout,file,status)
           endif
         endif
c
       endif
c
c  Scan the sample file
c
       if (option.eq.'SCAN') then
         call scan_wind_gauge(ifile,isamp1,isamp2,status)
c
c  Plot wind gauge readings
c
       elseif (option.eq.'PLOT') then
         call plot_wind_gauge(ifile,isamp1,isamp2,plot_device,status)
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_WIND_GAUGE')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


