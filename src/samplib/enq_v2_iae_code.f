

C+ ENQ_V2_IAE_CODE

       subroutine enq_v2_iae_code ( n, iae, s )
C
C returns the aerial code for the n'th aerial in the sample file
C
C Input
C   N            -      I4      -     aerial index
C
C Returned
C   IAE          -      I4      -     aerial designation
C   S            -      I4      -     error return
C
C CT Version 2 support for ENQ_IAE_CODE
C
C [PA, 4/11/88]
*-

       integer     s
       integer     n, iae

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.ge.1 .and. n.le.naes) then
         iae = iae_code(n)
       else
         s=ILL_AERIAL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_IAE_CODE' )

       end
