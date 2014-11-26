*+ enq_v2_mon_ifagc

       subroutine enq_v2_mon_ifagc( iae, agc, s)
C      ---------------------------------------
C
C Returns the ALC level for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   agc          -      r4      -     alc level
C   s            -      i4      -     error return
C
C The ALC values are 12-bit integers, normalised here to values
C in the range 0-100.
C
C CT Version 2 support for ENQ_mon_ifagc
C
C [DJT, 18/4/90; GP 30 Jan 2002]
*-

       integer     iae, s
       real        agc

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v2_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         agc = mon_IFalc( iae )/40.96
       else
         s=ill_aerial
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V2_mon_ifagc')

       end
