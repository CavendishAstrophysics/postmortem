C+ enq_mon_wind

       subroutine enq_mon_wind( lun, wind, s )
C      ---------------------------------------
C
C Returns the wind gauge reading for aerial iae for the current sample
C
C Given
C   lun              -          i4      -    logical unit number
C Returned
C   wind             -          r4      -    wind gauge reading (knots)
C   s                -          i4      -    error return
C
C Returns the wind gauge reading for the current sample.  The routine
C reads the entry direcly from the monitor block and should be preceded
C by a call to READ_MONITOR.
C-

       integer      lun, s
       real         wind

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .eq. 2) then
         call enq_v2_mon_wind( wind, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_WIND' )

       end



