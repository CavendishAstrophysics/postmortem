C+ enq_mon_cyro

       subroutine enq_mon_cryo( lun, iae, tcryo, s )
C      ---------------------------------------------
C
C Returns the cryo temperature for the iae aerial for the current sample
C
C Given
C   lun              -          i4         -        logical unit number
C   iae              -          i4         -        aerial number
C Returned
C   tcryo            -          r4         -        cryo temperature
C   s                -          i4         -        error return
C
C Returns the cryo temperature reading for aerial iae as a value in
C the range 0-100.  The routine reads the entry directly from the
C monitor block for the current sample, and should be preceded by a
C call to READ_MONITOR.
C-

       integer      lun, iae, s
       real         tcryo

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .eq. 2) then
         call enq_v2_mon_cryo( iae, tcryo, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_CRYO' )

       end

