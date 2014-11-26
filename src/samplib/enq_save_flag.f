

C+ENQ_SAVE_FLAG

       subroutine enq_save_flag ( lun, save_flag, s )
C
C Returns the saved (archived) status for the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C
C Returned
C   SAVE_FLAG    -      I4      -     archive status flag
C   S            -      I4      -     error return
C
C Returns the current archive status for the sample file.  Values for
C the save status flag are:
C
C     save_flag = 0       sample file not saved
C     save_flag = 1       sample file marked for archiving
C     save_flag = 2       sample file archived on magnetic tape
C     save_flag = 3       sample file modified since last archived
C
C [DJT, 8/2/90]
*-

       integer    s
       integer    lun, save_flag

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C Check status on entry
       if ( s .ne. 0 ) return

C Determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C Call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_saved( save_flag, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_saved( save_flag, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_SAVE_FLAG' )

       end
