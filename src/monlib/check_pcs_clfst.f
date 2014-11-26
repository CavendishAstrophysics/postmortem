

*+CHECK_PCS_CLFST

       subroutine check_pcs_clfst (file, plot_device, status)
C      ------------------------------------------------------
C
C  Executes the CHECK-PCS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C      STATUS        integer       status value
C
C  This command scans the sample file and reports changes in the path
C  compensation adjustments applied during the observation.  The log of
C  PC changes may be displayed for selected aerials, within a chosen
C  sample range.
C
C  The PC settings within a given sample range may also be plotted out.
C  Note that they are displayed for all aerials on axes with a range 0-150.
C
C  Alternatively, the complete set of PC values for all aerials can be
C  printed out for any chosen sample.
C
C  The STATUS value should be zero on entry.
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
       integer    ilist(max_aes), isamp1, isamp2, num_samp
       integer    ifile, iout, iold, maxc, nae
       integer    termi, termo
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
       option=' '
       if (status.eq.0) then
c
c  Prompt for further options
c
         if (io_yesno('Do you want to scan the sample file? ','no',
     :                                                     status)) then
           option='SCAN'
           maxc=0
c
         elseif (io_yesno('Do you want to plot PC values? ','no',
     :                                                     status)) then
           option='PLOT'
         elseif (io_yesno('Do you want to display PC values by sample?',
     :                                                'no',status)) then
           option='SHOW'
         endif
c
c    Prompt for aerial list and sample range
c
         if (option.ne.' ') then
           call get_aerials( ifile, 'aerial list:', 'all', list, ilist,
     :                       max_aes, nae, status )
           call chr_chucas(list)
           if (option.eq.'SCAN' .or. option.eq.'PLOT') then
             call enq_numsamp(ifile,1,num_samp,status)
             isamp1=1
             isamp2=num_samp
             call io_geti('first sample:','*',isamp1,status)
             call io_geti('last sample:','*',isamp2,status)
             isamp1=max(1,isamp1)
             isamp2=min(num_samp,isamp2)
           endif
         endif
c
c    Prompt for output file
c
         if (option.eq.'SCAN') then
           call io_opeout(iout,status)
           if (status.eq.0) then
             if (iout.ne.termo) write(iout,*)'CHECK-PCS'
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
         call scan_pcs_clfst(ifile,list,ilist,nae,isamp1,isamp2,
     :                                                  maxc,status)
c
c  Plot PC values for selected aerials
c
       elseif (option.eq.'PLOT') then
         call plot_pcs_clfst(ifile,list,ilist,nae,isamp1,isamp2,
     :                                           plot_device,status)
c
c  Print PC values for selected samples
c
       elseif (option.eq.'SHOW') then
         call show_pcs_clfst(ifile,list,ilist,nae,status)
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine CHECK_PCS_CLFST')
       endif
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
