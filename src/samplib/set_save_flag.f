

C+SET_SAVE_FLAG

       subroutine set_save_flag ( lun, save_flag, s )
C
C Resets the saved (archived) status for the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   SAVE_FLAG    -      I4      -     archive status flag
C
C Returned
C   S            -      I4      -     error return
C
C Resets the archive status for the sample file.  Values for the
C status flag are:
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

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call set_v1_saved( save_flag, s )
       elseif (ct_vers .eq. 2) then
         call set_v2_saved( save_flag, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine SET_SAVE_FLAG' )

       end
