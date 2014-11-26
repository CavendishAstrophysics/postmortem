

*$(3)  Monitor System Enquiry Routines

C+ enq_mon_agc

       subroutine enq_mon_agc( lun, iae, agc, s )
C      ------------------------------------------
C
C returns the agc level for the iae aerial for the current sample
C
C Given
C   lun              -          i4         -        logical unit number
C   iae              -          i4         -        aerial number
C Returned
C   agc              -          r4         -        agc level
C   s                -          i4         -        error return
C
C Returns the agc level for aerial iae as an integer. The routine
C reads the entry direcly from the monitor block for the current sample
C and should be preceded by a call to read_monitor.
C-

       integer      lun, iae, s
       real         agc

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_mon_agc( iae, agc, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_mon_agc( iae, agc, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_AGC' )

       end
