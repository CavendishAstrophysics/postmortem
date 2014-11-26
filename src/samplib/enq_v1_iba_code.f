

C+ ENQ_V1_IBA_CODE

       subroutine enq_v1_iba_code ( n, iba, s )
C
C returns the sub-band code for the n'th sub-band in the sample file
C
C Input
C   N            -      I4      -     sub-band index
C
C Returned
C   IBA          -      I4      -     sub-band designation
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_IBA_CODE
C
C [PA, 1/11/88]
*-

       integer     s
       integer     n, iba

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.eq.1) then
         iba = 1
       else
         s=ILL_SUBBAND
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_IBA_CODE' )

       end
