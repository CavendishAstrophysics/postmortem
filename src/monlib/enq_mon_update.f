


C+ enq_mon_update

       subroutine enq_mon_update( lun, update, s )
C      -------------------------------------------
C
C returns the update array from the monitor block
C
C Given
C   lun              -          i4         -        logical unit number
C Returned
C   update           -          i4(*)      -        update array
C   s                -          i4         -        error return
C
C Returns the update array
C-

       integer      lun, update(1), s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_mon_update( update, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_UPDATE' )

       end
