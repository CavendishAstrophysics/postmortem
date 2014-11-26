C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_gtvis(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_GTVIS (status)
C
C  Converts Ryle Telescope visibility gains file imported from ND system
C
C  DJT, 9 June 94
*-
       integer    status
c
       character  file*64
       integer    ifile, irec
       integer    num_rec
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_record.inc'
       include '/mrao/post/include/gt_include.inc'
c
       if (status.ne.0) return

c  Get logical sample file name

       call io_getfil('visibility gains table file : ', ' ', file,
     *                                                       status)

c  Open the gains table file

       call io_nxtlun(ifile,status)
       open (ifile, file=file, access='DIRECT', status='OLD',
     :                         recl=4*2560, iostat=status)

c  Read first record to get number of records in file

       if (status.eq.0) then
         irec = 1
         read(ifile, rec=irec, iostat=status) gt_iovis
         num_rec = gt_iovis(1)
       endif

c  Read each gains table record, convert floating-point variables to
c  IEEE format and rewrite.

       if (num_rec.gt.0 .and. status.eq.0) then
         irec = 2
  1      read(ifile, rec=irec, iostat=status) gt_iovis
         if (status.eq.0) then

           call util_rndr32(gt_vis_gains,gt_vis_gains,max_vis*2)

           write(ifile, rec=irec, iostat=status) gt_iovis
           irec=irec+1
           goto 1
         endif
         status=0
       endif

       close(ifile)

       if (status.ne.0) then
          call io_wrerr(status, 'in subroutine CONVERT_GTVIS')
       endif

       end
