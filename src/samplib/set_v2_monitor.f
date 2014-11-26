


C$(7b)  Control tables version 2 parameter setting routines.


C+SET_V2_MONITOR

       subroutine set_v2_monitor ( lun, isamp, s )
C
C Prime the monitor block for later use by enquiry routines
C
C Given
C  lun        -        i4         -       logical unit number
C  isamp      -        i4         -       sample number
C Returned
C  s          -        i4         -       status value
C
C Support routine for SET_MONITOR for version 2 control tables
*-

       integer lun, isamp, s

       include '/mrao/post/include/mon_v2_block.inc'

       call read_monitor ( lun, isamp, mon_length, mon_block, s )

       end
