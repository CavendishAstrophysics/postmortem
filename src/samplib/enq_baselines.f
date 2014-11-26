

C+ENQ_BASELINES

       subroutine enq_baselines ( lun,
     *                            num_vis,
     *                            vis_list,
     *                            base,
     *                            skew,
     *                            s         )
C
C     Returns the baselines for a visibility list from a sample file.
C
C     Given:
C         Sample file logical unit number
              integer             lun
C         Number of visibilities (spacings)
              integer             num_vis
C         Spacing list
              integer             vis_list( num_vis )

C     Returned:
C         Baseline coordinates for each spacing in wavelengths.
              real*8              base( 3, num_vis )
C         The skew of this baseline from the sidereal time system used
              real*8              skew
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Calculates the 3-d coordinates of the baselines associated with
C     the current spacing list. The coordinate system is, of course,
C     terrestial (not celestial), and is a normal, right handed, system
C     defined by the following:
C
C         Z (3)   axis is in the direction of the North Celestial Pole.
C                 + Z is to the North.
C         X (1)   axis is arranged that it is aligned along the
C                 projection of the "axis" of the telescope on the
C                 equatorial plane. The sole significance of this
C                 direction is  that it is taken as the direction
C                 spacing radii are calculated - and is used primarily
C                 for calculating ionospheric corrections and display
C                 purposes.
C                 + X is to the West.
C         Y (2)   axis is perpendicular to the other two.
C                 + Y is outwards from the Earths axis.
C
C         SKEW    relates the X coordinate to the sidereal time system
C                 used. It is the angle from the perpendicular to
C                 RA=0 plane to the X axis at the zero of sidereal
C                 time. It is anticlockwise positive looking down on
C                 the North Pole.
C
C     N.B.    The sidereal times for the Cambridge telescopes are
C             Greenwich sidereal times - so SKEW is the angle between
C             the X axis and a line of latitude at Greenwich.
C
C     NPR.    11 June 1987
C
C-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_base (num_vis, vis_list, base, skew, s)
      elseif (ct_vers .eq. 2) then
         call enq_v2_base (num_vis, vis_list, base, skew, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_BASELINES' )

      end
