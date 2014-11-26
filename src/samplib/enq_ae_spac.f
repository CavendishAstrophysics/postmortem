

C+ ENQ_AE_SPAC

       subroutine enq_ae_spac ( lun, n, iae1, iae2, s )
C
C return the aerials for the n'th spacing in the sample file
C
C Input
C   LUN           -     I4     -   logical unit number
C   N             -     I4     -   spacing index
C
C Returned
C   IAE1          -     I4     -   East Aerial
C   IAE2          -     I4     -   West Aerial
C   S             -     I4     -   Error return
C
C A direct enquiry is made to the ISPAE array in the sample file control
C tables.
C
C [PA, 1/11/88]
*-

       integer     s
       integer     lun, n, iae1, iae2

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_ae_spac( n, iae1, iae2, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_ae_spac( n, iae1, iae2, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_AE_SPAC' )

       end
