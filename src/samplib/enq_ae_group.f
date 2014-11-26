

C+ ENQ_AE_GROUP

       subroutine enq_ae_group ( lun, n, iae1, iae2, s )
C
C return the first and last aerials for the n'th group
C
C Input
C   LUN           -     I4     -   logical unit number
C   N             -     I4     -   aerial group index
C
C Returned
C   IAE1          -     I4     -   East Aerial
C   IAE2          -     I4     -   West Aerial
C   S             -     I4     -   Error return
C
C Computes the index numbers of the first and last aerials in group N.
C
C [PA, 3/12/88]
*-

       integer     s
       integer     lun, n, iae1, iae2
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
           iae1 = 0
           iae2 = 0
           if (n.ge.1 .and. n.le.max_huts) then
             iae1 = 7*(n-1) + 1
             if (n.gt.4) iae1 = 8*(n-5)+29
             iae2 = 7*n
             if (n.gt.4) iae2 = 8*(n-4)+28
           end if
         else
           s = ILL_TSCOPE
           goto 999
         end if

       elseif (ct_vers .eq. 2) then
         iae1 = 1
         iae2 = 8

       else
         s = ILL_CONTTAB
         goto 999

       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_AE_GROUP' )

       end
