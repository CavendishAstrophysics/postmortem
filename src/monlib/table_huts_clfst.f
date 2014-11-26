

*+TABLE_HUTS_CLFST

       subroutine table_huts_clfst (ifile, values, nvalues, prec,
     :                                                           status)
C      -----------------------------------------------------------------
C
C  Tabulates hut parameter values by switched hut pairs.
C
C  Given:
C      IFILE     integer     sample file logical unit number
C      VALUES    real(*)     array containing parameter values
C      NVALUES   integer     number of parameters values
C      PREC      integer     precision required in output format
C      STATUS    integer     status value
C
C  Routine to print out a set of parameter values as a table of
C  switched hut pairs.
C
C  A fixed point format (F8.n) is used for the output of each value,
C  where the fractional part is given by the parameter PREC.  Use
C  PREC=-1 for an integer representation.  The table is written to
C  the current output device.
C
C  The STATUS value should be zero on entry.
C
*-
       integer   ifile, nvalues, prec, status
       real      values(nvalues)
c
       include '/mrao/post/include/clfst_constants.inc'
c
       character line*80, fmt*6
       character hut_ident(max_huts)
       integer   ihut, ieast, iwest, iout, ivalue
       integer   l1, l2, ll
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
       fmt='(F8.n)'
       if (prec.ge.0 .and. prec.le.6) write(fmt(5:5),'(I1)')prec
c
       do ihut=1,neast_huts+nwest_huts
         hut_ident(ihut)=char(ihut+64)
       enddo
       write(iout,'(15X,8(A,A,3X))')
     :             ('Hut ',hut_ident(neast_huts+ihut),ihut=1,nwest_huts)
       write(iout,*)
c
c  Tabulate with shortest spacings at top left
c
       do ieast=neast_huts,1,-1
         write(line,'(3x,''Hut '',A,4X,8(''      . ''))')
     :                                                hut_ident(ieast)
         do iwest=1,nwest_huts
           l1=(iwest-1)*8+12
           l2=l1+7
           ivalue=(ieast-1)*nwest_huts+iwest
           if (prec.ge.0) then
             write(line(l1:l2),fmt)values(ivalue)
           else
             write(line(l1:l2),'(I8)')nint(values(ivalue))
           endif
         enddo
         ll=nwest_huts*8+12
         write(iout,*)line(1:ll)
       enddo
c
       write(iout,*)
c
       end
