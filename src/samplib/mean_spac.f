

C+MEAN_SPAC

       subroutine mean_spac ( lun, ilist, nspac, minsp, maxsp, avsp,
     :                                                          status )
C
C  Computes average spacing number for spacing list.
C
C  Given:
C      LUN       integer     sample file unit number
C      ILIST     integer     list of spacing index numbers
C      NSPAC     integer     number of spacings
C
C  Returned:
C      MINSP     integer     minimum spacing number
C      MAXSP     integer     maximum spacing number
C      AVSP      real        average spacing number
C      STATUS    integer     status value
C
C  Scans the spacings represented by the input spacing list, and computes
C  the minimum, maximum and average spacing numbers contained in the list.
C  The status value should be zero on entry, and is not changed.
C
*-
       integer  lun, nspac, ilist(nspac), minsp, maxsp, status
       integer  is, isp, isp1, ispac, n, nvis
       real*4   avsp, spac
       real*8   sumsp
c
       if (status.ne.0) return
       call enq_numvis( lun, nvis, status )
c
       n=0
       isp1=0
       minsp=0
       maxsp=0
       avsp=0.0
       sumsp=0.d0
       if (nspac.gt.0 .and. status.eq.0) then
         call enq_spac_no(lun,ilist(1),spac,status)
         minsp=nint(spac)
         maxsp=minsp
         avsp=spac
         do is=1,nspac
           isp=ilist(is)
           if (isp.ge.1 .and. isp.le.nvis .and. isp.ne.isp1) then
             call enq_spac_no(lun,isp,spac,status)
             ispac=nint(spac)
             if (ispac.lt.minsp) then
               minsp=ispac
             elseif (ispac.gt.maxsp) then
               maxsp=ispac
             endif
             sumsp=sumsp+spac
             n=n+1
           endif
           isp1=isp
         enddo
         if (n.gt.0) avsp=sumsp/n
       endif
c
       if (status.ne.0) call smp_wrerr(status,'in subroutine MEAN_SPAC')
c
       end
