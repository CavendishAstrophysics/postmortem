


C+ ENQ_ISP_CODE

       subroutine enq_isp_code ( lun, n, isp, s )
C
C returns the spacing code for the n'th spacing in the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   N            -      I4      -     spacing index
C
C Returned
C   ISP          -      I4      -     spacing designation
C   S            -      I4      -     error return
C
C returns the spacing designation for the n'th spacing in the sample
C file. The designation is returned as an integer code in the range
C given by 1 - the maximum baseline expressed as the nearest integer
C multiple of the spacing increment.
C
C [PA, 4/11/88]
*-

       integer    s
       integer    lun, n, isp

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_isp_code( n, isp, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_isp_code( n, isp, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_ISP_CODE' )

       end
