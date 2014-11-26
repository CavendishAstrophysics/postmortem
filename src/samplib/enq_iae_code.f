

C+ ENQ_IAE_CODE

       subroutine enq_iae_code ( lun, n, iae, s )
C
C returns the aerial code for the n'th aerial in the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   N            -      I4      -     aerial index
C
C Returned
C   IAE          -      I4      -     aerial designation
C   S            -      I4      -     error return
C
C returns the aerial designation for the n'th aerial in the sample
C file. The designation is returned as an integer code in the range
C given by 1 - the max_aes.
C
C [PA, 4/11/88]
*-

       integer    s
       integer    lun, n, iae

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_iae_code( n, iae, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_iae_code( n, iae, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_IAE_CODE' )

       end
