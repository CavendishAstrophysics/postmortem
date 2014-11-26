

*+ enq_v2_mon_rain

       subroutine enq_v2_mon_rain( iae, rain, s)
C      -----------------------------------------
C
C Returns the rain gauge reading for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   rain         -      r4      -     rain gauge reading
C   s            -      i4      -     error return
C
C CT Version 2 support for ENQ_MON_RAIN
C
C [DJT, 3/4/90]
*-

       integer     iae, s
       real        rain

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         rain = mon_rn( iae )/409.6
       else
         s=ILL_AERIAL
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_MON_RAIN')

       end
