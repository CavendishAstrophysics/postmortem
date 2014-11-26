

C+SET_MONITOR

       subroutine set_monitor ( lun, isamp, s )
C
C Prime the monitor block for later use by enquiry routines
C
C Given
C  lun        -        i4         -       logical unit number
C  isamp      -        i4         -       sample number
C Returned
C  s          -        i4         -       status value
C
*-

       integer lun, isamp, s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

       if ( s .ne. 0 ) return

       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

       if (ct_vers .le. 1) then
         call set_v1_monitor( lun, isamp, s )
       elseif (ct_vers .eq. 2) then
         call set_v2_monitor( lun, isamp, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine SET_MONITOR' )

       end
