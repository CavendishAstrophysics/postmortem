

*+TABLE_AES

       subroutine table_aes (ifile, values, ilist, nae, prec,
     :                                                        status)
C      --------------------------------------------------------------
C
C  Tabulates aerial parameter values.
C
C  Given:
C      IFILE     integer     sample file logical unit number
C      VALUES    real(*)     array containing parameter values
C      ILIST     integer(*)  array containing aerial numbers
C      NAE       integer     number of aerials
C      PREC      integer     precision required in output format
C      STATUS    integer     status value
C
C  Routine to tabulate a set of parameter values for the aerials
C  included in the given aerial list, as a table arranged by groups.
C  A fixed point format (F8.n) is used for the output of each value,
C  where the fractional part is given by the parameter PREC.  Use
C  PREC=-1 for an integer representation.  The table is written to
C  the current output device.
C
C  The STATUS value should be zero on entry.
C
*-
       integer   ifile, nae, ilist(nae), prec, status
       real      values(nae)
c
       character line*80, fmt*6
       integer   iae, iae1, iae2, ig, n_groups
       integer   i, ii, iout, l1, l2, ll
       logical   referred
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       fmt='(F8.n)'
       if (prec.ge.0 .and. prec.le.6) write(fmt(5:5),'(I1)')prec
c
c  Tabulate for each aerial group in turn
c
       call enq_groups( ifile, n_groups, status)
       do ig=1,n_groups
c
c    Check whether this hut is referred to in the aerial list
c
         referred=.false.
         call enq_ae_group( ifile, ig, iae1, iae2, status )
         do iae=iae1,iae2
           i=0
           do while (.not.referred .and. i.lt.nae)
             i=i+1
             if (iae.eq.ilist(i)) referred=.true.
           enddo
         enddo
c
c    List parameter values for this group
c
         if (referred) then
           write(line,'(A,I3,A,I2,2X,8(''      . ''))')
     :                                           ' ae',iae1,'-',iae2
           do iae=iae1,iae2
             do i=1,nae
               if (iae.eq.ilist(i)) then
                 ii=iae-iae1+1
                 l1=(ii-1)*8+12
                 l2=l1+7
                 if (prec.ge.0) then
                   write(line(l1:l2),fmt)values(i)
                 else
                   write(line(l1:l2),'(I8)')int(values(i))
                 endif
               endif
             enddo
           enddo
           ll=(iae2-iae1+1)*8+11
           write(iout,*)line(1:ll)
         endif
       enddo
c
       write(iout,*)
c
       end
