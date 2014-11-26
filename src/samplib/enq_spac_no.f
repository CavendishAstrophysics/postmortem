C+ENQ_SPAC_NO

       subroutine enq_spac_no ( lun, n, spac_no, s )
C
C Returns the spacing number for the n'th visibility in the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   N            -      I4      -     visibility index number
C
C Returned
C   SPAC_NO      -      R4      -     spacing number
C   S            -      I4      -     error return
C
C Returns the spacing number for the n'th visibility in the sample file,
C as a real number.
C
C This routine replaces the real function SPAC_NO.
C
C [DJT, 27/9/89]
*-

       integer    s
       integer    lun, n
       real*4     spac_no

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_spac_no( n, spac_no, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_spac_no( n, spac_no, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_SPAC_NO' )

       end


