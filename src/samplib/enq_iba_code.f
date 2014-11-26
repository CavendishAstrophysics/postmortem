

C+ ENQ_IBA_CODE

       subroutine enq_iba_code ( lun, n, iba, s )
C
C returns the sub-band code for the n'th sub-band in the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   N            -      I4      -     sub-band index
C
C Returned
C   IBA          -      I4      -     sub-band designation
C   S            -      I4      -     error return
C
C returns the sub-band designation for the n'th sub-band in the sample
C file. The designation is returned as an integer code in the range
C 1-max_subb which correspondes to the sub-band designations A-Z. For
C telescopes with only one sub-band the routine always returns unity.
C If the designation for an non-existent sub-band is requested then
C the S=ILL_SUBBAND.
C
C [PA, 1/11/88]
*-

       integer    s
       integer    lun, n, iba

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_iba_code( n, iba, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_iba_code( n, iba, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_IBA_CODE' )

       end
