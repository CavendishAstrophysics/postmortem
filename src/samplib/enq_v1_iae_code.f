

C+ ENQ_V1_IAE_CODE

       subroutine enq_v1_iae_code ( n, iae, s )
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
C CT Version 1 support for ENQ_IAE_CODE
C
C [PA, 4/11/88]
*-

       integer     s
       integer     n, iae

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       iae = n

       end
