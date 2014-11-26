

C+ ENQ_ICH_FREQ

       subroutine enq_ich_freq ( lun, ich, offset, s )
C
C Returns the frequency offset for a speficied channel.
C
C Input
C   LUN          -      I4      -     logical unit number
C   ICH          -      I4      -     channel index
C
C Returned
C   OFFSET       -      R8      -     channel frequency offset (Hz)
C   S            -      I4      -     error return
C
C Returns the frequency offset for the n'th channel in each sub-band.
C The frequency is returned in Hz as an offset from the centre
C frequency of the sub-band.  For telescopes with only one channel the
C routine always returns zero.  If the designation for an non-existent
C sub-band is requested then the S=ILL_CHANNEL.
C
C [DJT, 20/4/90]
*-

       integer    s
       integer    lun, ich
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
         call enq_v1_ich_freq( ich, offset, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_ich_freq( ich, offset, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_ICH_FREQ' )

       end
