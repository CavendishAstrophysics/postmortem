

C+ENQ_V2_FREQ

       subroutine enq_v2_freq ( centre_freq, s )
C
C     Returns the observing frequency from control tables.
C
C     Returned:
C         Nominal centre frequency (Hz).
               real*8      centre_freq
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_FREQ.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      integer   ich, isb, isgn, count

      if ( s .ne. 0 ) return

      isgn = 1
      if (LOhigh) isgn = -1

      count = 0
      centre_freq = 0.0
      do isb = 1,Nsubb
        do ich = 1,Nchannel
          count = count + 1
          centre_freq = centre_freq + freq
     *                              + isgn*subb_freq(iba_code(isb))
     *                              + chan_freq(ich_code(ich))
        end do
      end do
      centre_freq = centre_freq*1.0D+6/float(count)

      end
