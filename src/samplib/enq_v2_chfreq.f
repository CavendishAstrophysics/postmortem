

C+ENQ_V2_CHFREQ

       subroutine enq_v2_chfreq ( iba, ich, ch_freq, s )
C
C     Returns the observing frequency of a specified frequency channel
C
C     Given:
C         Sub-band index
               integer     iba
C         Channel index
               integer     ich
C
C     Returned:
C         Channel frequency (Hz).
               real*8      ch_freq
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_CHFREQ.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      integer   isgn

      if ( s .ne. 0 ) return

      isgn = 1
      if (LOhigh) isgn = -1

      ch_freq = freq + isgn*subb_freq(iba_code(iba))
     :               + chan_freq(ich_code(ich))
      ch_freq = ch_freq*1.0D+6

      end
