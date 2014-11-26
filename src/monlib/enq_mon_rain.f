


C+ enq_mon_rain

       subroutine enq_mon_rain( lun, iae, rain, s )
C      --------------------------------------------
C
C Returns the rain gauge reading for aerial iae for the current sample
C
C Given
C   lun              -          i4         -        logical unit number
C   iae              -          i4         -        aerial number
C Returned
C   rain             -          r4         -        rain gauge reading
C   s                -          i4         -        error return
C
C Returns the rain gauge reading for aerial IAE as an integer.  The
C routine reads the entry direcly from the monitor block for the current
C sample and should be preceded by a call to READ_MONITOR.
C-

       integer      lun, iae, s
       real         rain

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .eq. 2) then
         call enq_v2_mon_rain( iae, rain, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_RAIN' )

       end
