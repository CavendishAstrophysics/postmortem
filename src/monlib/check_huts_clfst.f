


*+CHECK_HUTS_CLFST

       subroutine check_huts_clfst (nhut_bad, status)
C      ----------------------------------------------
C
C  Checks hut status and reports problems.
C
C  Returned:
C      NHUT_BAD  integer     number of problems found
C      STATUS    integer     status value
C
C  Routine to check the hut status words and report any problems to
C  the output device.  The number of problem huts found is returned.
C  The following status bits are maintained during the observing run
C  and are investigated by this routine:
C
C  Hut status word,  bit 15 : set if hut on-line and operational
C                    bit 14 : set if hut receiving data
C                    bit 13 : set if aerials tracking
C                    bit 12 : set if hut temporarily off-line
C                    bit 11 : set if hut arguing
C                    bit 10 : set if local oscillator out of lock
C
C  The STATUS value should be zero on entry and is not changed.
C
*-
       integer  nhut_bad, status
C
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character  line*80, char
       integer    ihut, iout, l, n
c
       if (status.ne.0) return
c
       nhut_bad=0
       call io_enqout(iout)
c
c  Check huts operational
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).eq.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)line(1:l),' not operational'
       nhut_bad=nhut_bad+n
c
c  Check huts receiving data
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).ne.0 .and.
     :       btest(ihutstat(ihut),14).eq.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)line(1:l),' not receiving data'
       nhut_bad=nhut_bad+n
c
c  Check aerial tracking for each hut
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).ne.0 .and.
     :       btest(ihutstat(ihut),13).eq.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)line(1:l),' aerials not tracking'
       nhut_bad=nhut_bad+n
c
c  Check huts on-line throughout observing run
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).ne.0 .and.
     :       btest(ihutstat(ihut),12).ne.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)line(1:l),' temporarily off-line'
       nhut_bad=nhut_bad+n
c
c  Check micro communications ok during run
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).ne.0 .and.
     :       btest(ihutstat(ihut),11).ne.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)line(1:l),' arguing'
       nhut_bad=nhut_bad+n
c
c  Check local oscillators locked during run
c
       n=0
       l=7
       do ihut=1,max_huts
         if (btest(ihutstat(ihut),15).ne.0 .and.
     :       btest(ihutstat(ihut),10).ne.0) then
           line(l+2:l+2)=char(ihut+64)
           l=l+2
           n=n+1
         endif
       enddo
       if (n.gt.0) write(iout,*)
     :                      line(1:l),' local oscillator out of lock'
       nhut_bad=nhut_bad+n
c
       end
