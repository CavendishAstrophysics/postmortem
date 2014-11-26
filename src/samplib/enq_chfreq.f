

C+ENQ_CHFREQ

       subroutine enq_chfreq ( lun, iba, ich, ch_freq, s )
C
C     Returns the channel frequency for the specified channel.
C
C     Given:
C         Sample file Fortran logical unit number
              integer     lun
C         Sub-band index
              integer     iba
C         Channel index
              integer     ich
C
C     Returned:
C         Channel frequency (Hz)
               real*8     ch_freq
C         Status
              integer     s
C
C     PA      22 January 1990
C     DJT      8 August 1991
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_freq( ch_freq, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_chfreq( iba, ich, ch_freq, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_CHFREQ' )

      end
