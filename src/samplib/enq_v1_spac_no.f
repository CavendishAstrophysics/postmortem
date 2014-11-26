C+ENQ_V1_SPAC_NO

       subroutine enq_v1_spac_no ( n, spac_no, s )
C
C Returns the spacing number of the n'th visibility in the sample file
C
C Input
C   N            -      I4      -     visibility index number
C
C Returned
C   SPAC_NO      -      R4      -     spacing number
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_SPAC_NO
C
C [DJT, 27/9/89]
*-

       integer     s
       integer     n
       real*4      spac_no

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (n.ge.1 .or. n.le.nsp) then
         spac_no=( x(ispae(2,n)) - x(ispae(1,n)) )*spfac
       else
         s=ILL_SPAC
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_SPAC_NO' )

       end


