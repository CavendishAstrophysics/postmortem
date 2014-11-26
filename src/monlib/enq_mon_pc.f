C+ enq_mon_pc

       subroutine enq_mon_pc( lun, iae, ipc, s )
C      -----------------------------------------
C
C returns the pc level for the iae aerial for the current sample
C
C Given
C   lun              -          i4         -        logical unit number
C   iae              -          i4         -        aerial number
C Returned
C   ipc              -          i4         -        pc level
C   s                -          i4         -        error return
C
C Returns the pc level for aerial iae as an integer. The routine
C reads the entry direcly from the monitor block for the current sample
C and should be preceeded by a call to read_monitor.
C-

       integer      lun, iae, ipc, s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_mon_pc( iae, ipc, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_PC' )

       end
