


*$(3a) V1 support routines for monitor enquiry system

*+ enq_v1_mon_agc

       subroutine enq_v1_mon_agc( iae, agc, s)
C      ---------------------------------------
C
C returns the agc level for the current sample
C
C Input
C   iae          -      i4      -     aerial number
C
C Returned
C   agc          -      r4      -     agc level
C   s            -      i4      -     error return
C
C CT Version 1 support for ENQ_MON_AGC
C
C [PA, 9/11/88]
*-

       integer     iae, s
       real        agc

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/mon_v1_block.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if ( iae.ge.1 .and. iae.le.max_aes ) then
         agc = mon_agc( iae )
       else
         s=ill_aerial
         goto 999
       end if

       return

 999   call mon_wrerr( s, 'in subroutine ENQ_V1_MON_AGC')

       end
