

C+ ENQ_ICH_CODE

       subroutine enq_ich_code ( lun, n, ich, s )
C
C returns the channel code for the n'th channel in the sample file
C
C Input
C   LUN          -      I4      -     logical unit number
C   N            -      I4      -     sub-band index
C
C Returned
C   ICH          -      I4      -     channel designation
C   S            -      I4      -     error return
C
C returns the sub-band designation for the n'th sub-band in the sample
C file. The designation is returned as an integer code in the range
C 1-max_channel. For telescopes with only one sub-band the routine always
C returns unity. If the designation for an non-existent sub-band is requested
C then the S=ILL_CHANNEL.
C
C [PA, 1/11/88]
*-

       integer    s
       integer    lun, n, ich

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_ich_code( n, ich, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_ich_code( n, ich, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_ICH_CODE' )

       end
