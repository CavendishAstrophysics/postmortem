

C+ ENQ_V2_IBA_FREQ

       subroutine enq_v2_iba_freq ( iba, sb_freq, s )
C
C Returns the frequency offset for a specified sub-band
C
C Input
C   IBA          -      I4      -     sub-band index
C
C Returned
C   SB_FREQ      -      R8      -     sub-band frequency offset (Hz)
C   S            -      I4      -     error return
C
C CT Version 2 support for ENQ_IBA_FREQ
C
C [DJT, 20/4/90]
*-

       integer     s
       integer     iba
       real*8      sb_freq

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       if (iba.ge.1 .and. iba.le.Nsubb) then
         sb_freq = subb_freq(iba_code(iba))*1.D6
         if (LOhigh) sb_freq = -sb_freq
       else
         s=ILL_SUBBAND
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_IBA_FREQ' )

      end
