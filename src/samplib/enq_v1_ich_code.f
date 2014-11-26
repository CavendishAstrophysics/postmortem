

C+ ENQ_V1_ICH_CODE

       subroutine enq_v1_ich_code ( n, ich, s )
C
C returns the channel code for the n'th channel in the sample file
C
C Input
C   N            -      I4      -     channel index
C
C Returned
C   ICH          -      I4      -     channel designation
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_ICH_CODE
C
C [PA, 1/11/88]
*-

       integer     s
       integer     n, ich

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.eq.1) then
         ich = 1
       else
         s=ILL_CHANNEL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_ICH_CODE' )

       end
