C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_gt(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_GT (status)
C
C  Converts Ryle Telescope gains file imported from ND system
C
C  DJT, 9 June 94
*-
       integer    status
c
       character  file*64
       integer    ifile, irec, i
       integer    num_rec
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_record.inc'
       include '/mrao/post/include/gt_include.inc'
c
       integer    ngains
       parameter (ngains = max_channel*max_subb*max_RT_aes)
c
       if (status.ne.0) return

c  Get logical sample file name

       call io_getfil('gains table file : ', ' ', file, status)

c  Open the gains table file

       call io_nxtlun(ifile,status)
       open (ifile, file=file, access='DIRECT', status='OLD',
     :                         recl=1024*4, iostat=status)

c  Read first record to get number of records in file

       if (status.eq.0) then
         irec = 1
         read(ifile, rec=irec, iostat=status) gt_times
         num_rec = gt_num_rec
       endif

c  Read each gains table record, realign floating-point variables to
c  word boundaries, convert to IEEE format and rewrite.

       if (status.eq.0) then
         irec = 2
  1      read(ifile, rec=irec, iostat=status) gt_io
         if (status.eq.0) then

           do i = 160, 149, -1
             gt_record(i) = gt_record(i-2)
           enddo
           do i = 147, 53, -1
             gt_record(i) = gt_record(i-1)
           enddo

           call util_rndr64(gt_freq,gt_freq,1)
           call util_rndr64(gt_subb_freq,gt_subb_freq,5)
           call util_rndr64(gt_chan_freq,gt_chan_freq,8)
           call util_rndr64(gt_ra,gt_ra,1)
           call util_rndr64(gt_dec,gt_dec,1)

c    Convert calibration record

           do i = 1, cal_length
             cal_record(i) = 0
           enddo
           do i = 1, 11
             cal_record(i) = gt_cal_record(i)
           enddo
           do i = 12, 74
             cal_record(i+1) = gt_cal_record(i)
           enddo

           call util_rndr64(cal_refdat,cal_refdat,1)
           call util_rndr32(cal_bandwidth,cal_bandwidth,1)
           call util_rndr32(cal_integt,cal_integt,1)
           call util_rndr64(cal_ra,cal_ra,6)
           call util_rndr64(cal_dec,cal_dec,6)
           call util_rndr32(cal_src_flux,cal_src_flux,6)
           call util_rndr32(cal_mod_mean,cal_mod_mean,1)
           call util_rndr32(cal_mod_sigma,cal_mod_sigma,1)

           do i = 1, cal_length
             gt_cal_record(i) = cal_record(i)
           enddo

c    Convert aerial gains

           call util_rndr32(gt_ae_gains,gt_ae_gains,ngains*2)

           write(ifile, rec=irec, iostat=status) gt_io
           irec=irec+1
           goto 1
         endif
         status=0
       endif

       close(ifile)

       if (status.ne.0) then
          call io_wrerr(status, 'in subroutine CONVERT_GT')
       endif

       end
