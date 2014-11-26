


*+ enq_v1_mon_pc

       subroutine enq_v1_mon_pc( iae, ipc, s)
C      --------------------------------------
C
C returns the pc level for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   ipc          -      i4      -     pc level
C   s            -      i4      -     error return
C
C CT Version 1 support for ENQ_MON_PC
C
C [PA, 9/11/88]
*-

       integer     iae, ipc, s

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v1_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         ipc = mon_pc( iae )
       else
         s=ill_aerial
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V1_MON_PC')

       end
