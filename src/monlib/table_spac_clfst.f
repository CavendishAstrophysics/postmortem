

*+TABLE_SPAC_CLFST

       subroutine table_spac_clfst (ifile, values, ilist, nspac,
     :                              prec, marginals, status     )
C      ---------------------------------------------------------
C
C  Tabulates spacing parameter values by aerial number.
C
C  Given:
C      IFILE     integer     sample file logical unit number
C      VALUES    real(*)     array containing parameter values
C      ILIST     integer(*)  array containing spacing index numbers
C      NSPAC     integer     number of spacings
C      PREC      integer     precision required for output format
C      MARGINALS logical     print marginals control flag
C      STATUS    integer     status value
C
C  Routine to tabulate a set of parameter values for the spacings
C  included in the given spacing list, as a cross-reference table
C  by aerial number tabulating Easterly against Westerly aerials.
C  A fixed point format (F8.n) is used for the output of each value,
C  where the fractional part is given by the parameter PREC.  Use
C  PREC=-1 for an integer representation.  If the MARGINALS control
C  flag is set, then row and column means will be included in the
C  table.  The table is written to the current output device.
C
C  The STATUS value should be zero on entry.
C
*-
       integer   ifile, nspac, ilist(nspac), prec, status
       real      values(nspac)
       logical   marginals
c
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character line*80, fmt*6
       real      emean(8), wmean
       integer   neast(8), nwest
       integer   iae, iae1, iae2, iaw, iaw1, iaw2, iw
       integer   ii, ihute, ihutw, iout, ispac, l1, l2
       logical   referred
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       fmt='(F8.n)'
       if (prec.ge.0 .and. prec.le.6) write(fmt(5:5),'(I1)')prec
c
c  Tabulate for each Westerly aerial group (hut) in turn
c
       do ihutw=neast_huts+1,max_huts
c
c    Check whether this Westerly hut is referred to in the spacing list
c
         referred=.false.
         call enq_ae_hut(ifile,ihutw,iaw1,iaw2,status)
         do iaw=iaw1,iaw2
           ispac=0
           neast(iaw-iaw1+1)=0
           emean(iaw-iaw1+1)=0.0
           do while (.not.referred .and. ispac.lt.nspac)
             ispac=ispac+1
             if (iaw.eq.ispae(2,ilist(ispac))) referred=.true.
           enddo
         enddo
c
         if (referred) then
           write(line,'(A,8(I7,1X))')'aerial',(iaw,iaw=iaw1,iaw2)
           if (marginals) line(77:)='mean'
           write(iout,*)line
c
c    Check whether Easterly huts are referred to in the spacing list
c
           do ihute=neast_huts,1,-1
             referred=.false.
             call enq_ae_hut(ifile,ihute,iae1,iae2,status)
             do iae=iae1,iae2
               ispac=0
               do while (.not.referred .and. ispac.lt.nspac)
                 ispac=ispac+1
                 if (iae.eq.ispae(1,ilist(ispac))) referred=.true.
               enddo
             enddo
c
c      List parameter values for this hut, accumulate marginals
c
             if (referred) then
               write(iout,*)
               do iae=iae2,iae1,-1
                 write(line,'(I4,2X,8(''      . ''))')iae
                 nwest=0
                 wmean=0.0
                 do iw=iaw1,iaw2
                   do ispac=1,nspac
                     if (iae.eq.ispae(1,ilist(ispac)) .and.
     :                    iw.eq.ispae(2,ilist(ispac))) then
                       ii=iw-iaw1+1
                       l1=(ii-1)*8+7
                       l2=l1+7
                       if (prec.ge.0) then
                         write(line(l1:l2),fmt)values(ispac)
                       else
                         write(line(l1:l2-1),'(I7)')int(values(ispac))
                       endif
                       emean(ii)=emean(ii)+values(ispac)
                       wmean=wmean+values(ispac)
                       neast(ii)=neast(ii)+1
                       nwest=nwest+1
                     endif
                   enddo
                 enddo
                 if (nwest.gt.0) wmean=wmean/float(nwest)
                 if (marginals) write(line(71:),'(F10.1)')wmean
                 write(iout,*)line
               enddo
               if (io_attn(status)) goto 1
c
             endif
           enddo
c
c    Print Easterly marginals
c
           if (marginals) then
             do iaw=iaw1,iaw2
               ii=iaw-iaw1+1
               if (neast(ii).gt.0) emean(ii)=emean(ii)/float(neast(ii))
             enddo
             write(iout,'(/2X,A,8F8.1)')'mean ',emean
           endif
           write(iout,*)
c
         endif
       enddo
c
    1  continue
c
       end
