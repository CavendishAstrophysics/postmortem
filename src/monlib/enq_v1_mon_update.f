

*+ enq_v1_mon_update

       subroutine enq_v1_mon_update( update, s)
C      ----------------------------------------
C
C returns the update array
C
C Input
C   None
C Returned
C   update       -      i4(*)   -     update array
C   s            -      i4      -     error return
C
C CT Version 1 support for ENQ_MON_UPDATE
C
C [PA, 3/12/88]
*-

       integer     update(1), s
       integer     i

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v1_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       do i = 1,4
         update(i) = mon_update(i)
       end do

       end
