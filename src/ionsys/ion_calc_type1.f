C
C+ion_calc_type1
C
      SUBROUTINE ion_calc_type1( map, search_posn, ion, s )

C     Calculates ionospheric correction type 1 given a 1-D map.
C
C     Given:
C         The one dimensional map.
              real            map(*)
C         The pixel in the map to base the search around.
              integer         search_posn
C
C     Returned:
C         The ionospheric correction buffer
              real            ion(*)
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Calculates the ionospheric correction for a given one dimensional
C     map using the information currently in the ionospheric
C     correction runtime common block.
C
C     This is basically a 1-D source finding routine with the slight
C     addition that the correction is calculated from the distance
C     the source is from the map centre. The algorithm basically just
C     finds the maximum on the map.
C
C     NPR     11 November 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/clfst_constants.inc'
      include  '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variable declarations
C         General purpose loop counters and character string
              integer         i
C         Radius of search in pixels
              integer         search_size
C         Index in the map of the map maximum.
              integer         map_max
C         Calculation variables.
              real            y1, y2, y3, a, b, dist, amp, corr

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Find map maximum
      map_max     = search_posn
      search_size = int(search_rad/arcsec_per_gp)+1
      do 100, i = search_posn-search_size, search_posn+search_size
          if (map(i).gt.map(map_max)) map_max = i
  100 continue

C     Find the peak centre by fitting a parabola to
C     the three points around the peak maximum.
      y1 = map( map_max - 1 )
      y2 = map( map_max )
      y3 = map( map_max + 1 )

      a   = y1+y3-2.0*y2
      b   = y1-y3
      amp = y2 + (b*b)/(8.0*a)
      dist= (map_max - (map_size/2+1) + b/(2.0*a))*arcsec_per_gp
      corr= sin(dist*const_sa2r)*const_2pi

C     Write correction away...
      ion(1) = corr
      ion(2) = 0.0
      ion(3) = amp
      ion(4) = 0.0
      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_calc_type1 ' )
          return
      end
