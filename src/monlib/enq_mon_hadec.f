
C+ enq_mon_hadec

       subroutine enq_mon_hadec( lun, iae, iha, idec, s )
C      --------------------------------------------------
C
C Returns the ha and dec encoders (digitised) required and actual values
C
C Given
C   lun              -          i4         -        logical unit number
C   iae              -          i4         -        aerial number
C Returned
C   iha              -        2*i4         -        ha values
C   idec             -        2*i4         -        dec values
C   s                -          i4         -        error return
C
C Returns the required and actual ha and dec encoder values.
C-

       integer      lun, iae, iha(2), idec(2), s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_mon_hadec( iae, iha, idec, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_mon_hadec( iae, iha, idec, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_MON_HADEC' )

       end
