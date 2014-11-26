



*+TABLE_VIS_RYLE

       subroutine table_vis_ryle (sf_lun, values, ilist, nvis, prec,
     :                                                         status )
C      ----------------------------------------------------------------
C
C  Tabulates un-merged visibility data.
C
C  Tabulates visibility parameter values by spacing and sub-band versus
C  frequency channel.
C
C  Given:
C      SF_LUN      integer     sample file logical unit number
C      VALUES      real(*)     array containing parameter values
C      ILIST       integer(*)  array containing visibility index numbers
C      NVIS        integer     number of visibilities
C      PREC        integer     precision required for output format
C      STATUS      integer     status value
C
C  Routine to tabulate a set of parameter values for the visibilities
C  included in the given visibility list.
C
C  A fixed point format (F8.n) is used for the output of each value,
C  where the fractional part is given by the parameter PREC.  Use
C  PREC=-1 for an integer representation.
C
C  The table is written to the current output device.
C
C  The STATUS value should be zero on entry.
C
C  [PA, 28/11/88]
C  [DJT, 10/1/90]
*-
       integer   sf_lun, nvis, prec, status
       integer   ilist(nvis)
       real      values(nvis)

       include '/mrao/include/chrlib_functions.inc'

       character line*80, fmt*6
       character sub_bands(5)*1
       integer   isp1, ieast1, iwest1, ib1, ich
       integer   isp2, ieast2, iwest2, ib2
       integer   nae, nsp, nba, nch
       integer   i, iout, l1, l2

       data      sub_bands/'A','B','C','D','E'/

C check status on entry
       if (status.ne.0 .or. nvis.le.0) return

C find output device
       call io_enqout(iout)

C define format
       fmt='(F8.n)'
       if (prec.ge.0 .and. prec.le.6) write(fmt(5:5),'(I1)')prec

C find telescope configuration
       call enq_obsdef(sf_lun,nae,nsp,nba,nch,status)

C construct table
       ib1 = 0
       isp1 = 0
       ieast1 = 0
       iwest1 = 0
       line = ' '
       write(iout,'(4X,A,2X,8(4X,''ch'',I2))') 'spacing',(ich,ich=1,nch)
       do i = 1, nvis
         call enq_ae_vis(sf_lun,ilist(i),ieast2,iwest2,status)
         call enq_vis_desig(sf_lun,ilist(i),isp2,ib2,ich,status)
         if (isp1.ne.isp2 .or. ib1.ne.ib2 .or.
     :       ieast1.ne.ieast2 .or. iwest1.ne.iwest2) then
           ieast1 = ieast2
           iwest1 = iwest2
           isp1 = isp2
           ib1 = ib2
           write(iout,'(1X,A)')line(1:chr_lenb(line))
           write(line,'(I3,'' ae'',I2,'','',I1,1X,A1,8(''      . ''))')
     :                                 isp1,ieast1,iwest1,sub_bands(ib1)
         endif
         l1=(ich-1)*8+13
         l2=l1+7
         if (prec.ge.0) then
           write(line(l1:l2),fmt)values(i)
         else
           write(line(l1:l2-1),'(I7)')int(values(i))
         endif
       end do
       write(iout,'(1X,A/)')line(1:chr_lenb(line))

       end
