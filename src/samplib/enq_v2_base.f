

C+ENQ_V2_BASE

       subroutine enq_v2_base ( num_vis,
     *                          vis_list,
     *                          base,
     *                          skew,
     *                          s        )
C
C     Returns the baselines of a visibility list from control tables.
C
C     Given:
C         Number of visibilities (spacings)
              integer             num_vis
C         Spacing list
              integer             vis_list( num_vis )

C     Returned:
C         Baseline coordinates for each visibility in wavelengths.
              real*8              base( 3, num_vis )
C         The skew of this baseline from the sidereal time system used
              real*8              skew
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Control tables version 2 support routine for ENQ_BASELINES.
C
C-
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

C     Loop counters
          integer*4      i, j
C     Local index for channel, sub-band, spacing and visibility
          integer        ichan, isubb, isp, ivis
C     East and west aerial number for each spacing.
          integer*4      e_aerial, w_aerial

      if ( s .ne. 0 ) return

      skew  = elong - azdatum

      do i = 1, num_vis
          ivis = vis_list(i)
          isp = (ivis-1)/(Nsubb*Nchannel) + 1
          isubb = (ivis-(isp-1)*Nsubb*Nchannel-1)/Nchannel + 1
          ichan = (ivis-(isp-1)*Nsubb*Nchannel-(isubb-1)*Nchannel)

          do j = 1, Naes
              if ( iae_code(j) .eq. ispae(1, isp) ) e_aerial = j
              if ( iae_code(j) .eq. ispae(2, isp) ) w_aerial = j
          end do

          base( 1, i ) = dble(x(ichan, isubb, w_aerial))
     :                 - dble(x(ichan, isubb, e_aerial))
          base( 2, i ) = dble(y(ichan, isubb, w_aerial))
     :                 - dble(y(ichan, isubb, e_aerial))
          base( 3, i ) = dble(z(ichan, isubb, w_aerial))
     :                 - dble(z(ichan, isubb, e_aerial))

      end do

      end
