

C+ENQ_V1_BASE

       subroutine enq_v1_base ( num_vis,
     *                          vis_list,
     *                          base,
     *                          skew,
     *                          s         )
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
C     Control tables version 1 support routine for ENQ_BASELINES.
C
C     NPR.    11 June 1987
C
C-
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

C     Loop counter
          integer*4       i
C     East and west aerial number for each spacing.
          integer*4       e_aerial, w_aerial

      if ( s .ne. 0 ) return

      skew  = along*radps -
     *        (azim(1)+(azim(2)+azim(3)/60.)/60.)*const_d2r

      do 10, i = 1, num_vis
          e_aerial = ispae( 1, vis_list( i ))
          w_aerial = ispae( 2, vis_list( i ))

          base( 1, i ) = dble(x(w_aerial)) - dble(x(e_aerial))
          base( 2, i ) = dble(y(w_aerial)) - dble(y(e_aerial))
          base( 3, i ) = dble(z(w_aerial)) - dble(z(e_aerial))
  10  continue

      end
