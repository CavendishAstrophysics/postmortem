

C+ ENQ_V1_IBA_FREQ

       subroutine enq_v1_iba_freq ( iba, offset, s )
C
C Returns the frequency offset for the n'th sub-band in the sample file
C
C Input
C   IBA          -      I4      -     sub-band index
C
C Returned
C   OFFSET       -      R8      -     sub-band frequency offset (Hz)
C   S            -      I4      -     error return
C
C CT Version 1 support for ENQ_IBA_FREQ
C
C [DJT, 20/4/90]
*-

       integer     s
       integer     iba
       real*8      offset

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (iba.eq.1) then
         offset = 0
       else
         s=ILL_SUBBAND
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_IBA_FREQ' )

       end
