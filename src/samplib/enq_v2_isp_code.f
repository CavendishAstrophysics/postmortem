

C+ ENQ_V2_ISP_CODE

       subroutine enq_v2_isp_code ( n, isp, s )
C
C returns the spacing code for the n'th spacing in the sample file
C
C Input
C   N            -      I4      -     spacing index
C
C Returned
C   ISP          -      I4      -     spacing designation
C   S            -      I4      -     error return
C
C CT Version 2 support for ENQ_ISP_CODE
C
C [PA, 4/11/88]
*-

       integer     s
       integer     n, isp

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.ge.1 .and. n.le.nsp) then
         isp = isp_code(n)
       else
         s=ILL_SPAC
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_ISP_CODE' )

       end
