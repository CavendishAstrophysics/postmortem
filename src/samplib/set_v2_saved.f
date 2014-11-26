
C+SET_V2_SAVED

       subroutine set_v2_saved ( flag, s )
C
C Resets the saved (archived) status for the sample file
C
C Input
C   FLAG         -      I4      -     save status flag
C
C Returned
C   S            -      I4      -     error return
C
C CT Version 2 support for SET_SAVE_FLAG
C
C [DJT, 8/2/90]
*-

       integer     s
       integer     flag

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'

C check status on entry
       if ( s .ne. 0 ) return

       save_flag = flag

       end
