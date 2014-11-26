

C+ ENQ_HUT_AE

       subroutine enq_hut_ae ( lun, n, ihut, s )
C
C Returns the hut index appropriate to a given aerial
C
C Input
C   LUN           -     I4     -   logical unit number
C   N             -     I4     -   aerial number
C
C Returned
C   IHUT          -     I4     -   hut index number
C   S             -     I4     -   status value
C
C Computes the hut index corresponding to aerial number N.
C If N does not correspond to a valid aerial the returned index is 0.
C If the current telescope type is not CLFST then ILL_TSCOPE is returned.
C
C [DJT, 21/12/89]
*-

       integer     s
       integer     lun, n, ihut
       integer     itscope

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_phys_tscope( lun, itscope, s )
         if (itscope.eq.clfst) then
           ihut = 0
           if (n.ge.1 .and. n.le.neast_aes) then
             ihut = (n-1)/7 + 1
           elseif (n.gt.neast_aes .and. n.le.max_aes) then
             ihut = (n-neast_aes-1)/8 + neast_huts + 1
           end if
         else
           s = ILL_TSCOPE
           goto 999
         end if

       elseif (ct_vers .eq. 2) then
         s = ILL_TSCOPE

       else
         s = ILL_CONTTAB
         goto 999

       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_HUT_AE' )

       end
