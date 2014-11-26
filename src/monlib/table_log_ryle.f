


*+TABLE_LOG_RYLE

       subroutine table_log_ryle (sf_lun, values, ilist, nvis, prec,
     :                                                         status )
C      ----------------------------------------------------------------
C
C  Tabulates visibility data after a 'logical-aerial' merge.
C
C  Tabulates parameter values obtained by a 'logical-aerial' merge, by
C  aerial and sub-band versus frequency channel.
C
C  Given:
C      SF_LUN      integer     sample file logical unit number
C      VALUES      real(*)     array containing parameter values
C      ILIST       integer(*)  array containing merge index numbers
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
C  [DJT, 15/1/90]
*-
       integer   sf_lun, nvis, prec, status
       integer   ilist(nvis)
       real      values(nvis)

       include '/mrao/include/chrlib_functions.inc'

       character line*80, fmt*6
       character sub_bands(5)*1
       integer   ia, ib, ic, iae1, iae2, iba1, iba2, ich
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
       iae1 = 0
       iba1 = 0
       line = ' '
       write(iout,'(13X,8(4X,''ch'',I2))') (ich,ich=1,nch)
       do i = 1, nvis
         ia = (ilist(i)-1)/(nch*nba)+1
         ib = (ilist(i) - (ia-1)*nch*nba - 1)/nch + 1
         ic = ilist(i) - (ia-1)*nch*nba - (ib-1)*nch
         call enq_iae_code(sf_lun,ia,iae2,status)
         call enq_iba_code(sf_lun,ib,iba2,status)
         call enq_ich_code(sf_lun,ic,ich,status)
         if (iae1.ne.iae2 .or. iba1.ne.iba2) then
           iae1 = iae2
           iba1 = iba2
           write(iout,'(1X,A)')line(1:chr_lenb(line))
           write(line,'('' ae'',I2,'' sb '',A1,8(''      . ''))')
     :                                              iae1,sub_bands(iba1)
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
