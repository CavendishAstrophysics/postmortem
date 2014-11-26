

C+ENQ_V1_ICH_FREQ

       subroutine enq_v1_ich_freq ( ich, offset, s )
C
C Returns the frequency offset for a speficied channel
C
C Input
C   ICH          -      I4      -     channel index
C
C Returned
C   OFFSET       -      R8      -     channel frequency offset (Hz)
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_ICH_FREQ.
C
C [DJT, 20/4/90]
*-

       integer     s
       integer     ich
       real*8      offset

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (ich.eq.1) then
         offset = 0.d0
       else
         s=ILL_CHANNEL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_ICH_FREQ' )

       end
