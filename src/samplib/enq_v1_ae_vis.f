

C+ ENQ_V1_AE_VIS

       subroutine enq_v1_ae_vis ( n, iae1, iae2, s )
C
C returns the aerials for the n'th visibility in the sample file
C
C Input
C   N             -     I4     -   visibility index
C
C Returned
C   IAE1          -     I4     -   East Aerial
C   IAE2          -     I4     -   West Aerial
C   S             -     I4     -   Error return
C
C CT Version 1 support for ENQ_AE_VIS
C
C [PA, 4/11/88]
*-

       integer      s
       integer      n, iae1, iae2

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.ge.1 .and. n.le.nsp) then
         iae1 = ispae(1,n)
         iae2 = ispae(2,n)
       else
         s=ILL_SPAC
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_AE_VIS' )

       end
