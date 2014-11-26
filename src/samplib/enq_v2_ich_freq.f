

C+ ENQ_V2_ICH_FREQ

       subroutine enq_v2_ich_freq ( ich, ch_freq, s )
C
C Returns the frequency offset for a specified channel
C
C Input
C   ICH          -      I4      -     channel index
C
C Returned
C   CH_FREQ      -      R8      -     channel frequency offset (Hz)
C   S            -      I4      -     error return
C
C CT Version 2 support for ENQ_ICH_FREQ
C
C [DJT, 20/4/90]
*-

       integer     s
       integer     ich
       real*8      ch_freq

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (ich.ge.1 .and. ich.le.Nchannel) then
         ch_freq = chan_freq(ich_code(ich))*1.D6
       else
         s=ILL_CHANNEL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_ICH_FREQ' )

       end
