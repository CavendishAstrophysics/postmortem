C+ enq_mon_misc

       subroutine enq_mon_misc( lun, ipar, value, s )
C      ----------------------------------------------
C
C Returns miscellaneous parameter reading for the current sample
C
C Given
C   lun              -          i4         -        logical unit number
C   ipar             -          i4         -        parameter number
C Returned
C   value            -          r4         -        parameter reading
C   s                -          i4         -        error return
C
C Returns the parameter reading as a real value.  The routine reads the
C value direcly from the monitor block for the current sample and should
C be preceded by a call to READ_MONITOR.
C-

       integer      lun, ipar, s
       real         value

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .eq. 2) then
         call enq_v2_mon_misc( ipar, value, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_MISC' )

       end



