

C+ ENQ_IBA_FREQ

       subroutine enq_iba_freq ( lun, iba, offset, s )
C
C Returns the frequency offset for a specified sub-band.
C
C Input
C   LUN          -      I4      -     logical unit number
C   IBA          -      I4      -     sub-band index
C
C Returned
C   OFFSET       -      R8      -     sub-band frequency offset (Hz)
C   S            -      I4      -     error return
C
C Returns the frequency offset for the n'th sub-band in the sample
C file. The offset is returned in Hz, relative to the nominal
C observing frequency.
C
C [DJT, 20/4/90]
*-

       integer    s
       integer    lun, iba
       real*8     offset

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_iba_freq( iba, offset, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_iba_freq( iba, offset, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_IBA_FREQ' )

       end
