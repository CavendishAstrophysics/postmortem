


C+ENQ_V1_SAVED

       subroutine enq_v1_saved ( flag, s )
C
C Returns the saved (archived) status for the sample file
C
C Returned
C   FLAG         -      I4      -     save status flag
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_SAVE_FLAG
C
C [DJT, 8/2/90]
*-

       integer     s
       integer     flag

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'

C check status on entry
       if ( s .ne. 0 ) return

       flag = isaved

       end
