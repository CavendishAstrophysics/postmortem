

*+TABLE_SPAC_RYLE

       subroutine table_spac_ryle (sf_lun, values, ilist, n_group,
     :                                              prec, status )
C      -----------------------------------------------------------
C
C  Tabulates spacing parameter values by aerial number.
C
C  Given:
C      SF_LUN      integer     sample file logical unit number
C      VALUES      real(*)     array containing parameter values
C      ILIST       integer(*)  array containing group index numbers
C      N_GROUP     integer     number of spacings
C      PREC        integer     precision required for output format
C      STATUS      integer     status value
C
C  Routine to tabulate a set of parameter values for the spacings
C  included in the given spacing list, as a cross-reference table
C  by aerial number.
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
C  [DJT, 16/1/90]
*-
       integer   sf_lun, prec, status
       integer   n_group
       integer   ilist(n_group)
       real      values(n_group)

       include '/mrao/post/include/5km_constants.inc'

       character line*80, fmt*6
       integer   iae, iaw, iae1, iae2
       integer   i, iout, l1, l2

C check status on entry
       if (status.ne.0) return

C find output device
       call io_enqout(iout)

C define format
       fmt='(F8.n)'
       if (prec.ge.0 .and. prec.le.6) write(fmt(5:5),'(I1)')prec

C construct title
       line = ' '
       write(line,'(2X,A,8(I5,3X))')'aerial',(iaw,iaw=1,max_aes)
       write(iout,'(X,A/)')line

C construct table
       do iae=1,max_aes
         write(line,'(4X,I2,8(''      . ''))')iae
         do iaw=1,max_aes
           do i=1,n_group
             call enq_ae_vis(sf_lun,ilist(i),iae1,iae2,status)
             if (iae1.eq.iae) then
                l1=(iae2-1)*8+7
                l2=l1+7
                if (prec.ge.0) then
                  write(line(l1:l2),fmt) values(i)
                else
                  write(line(l1:l2-1),'(I7)') int(values(i))
                endif
             end if
           enddo
         enddo
         write(iout,'(1x,a)') line
       end do
       end
