C
C+ion_calc_type2
C
      SUBROUTINE ion_calc_type2( map, search_posn, ion, s )

C     Calculates an ionospheric correction given a 1-D map.
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
C     the source is from the map centre.
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
C         Calculation variables.
              real            sum_amp, sum_amp_dist, dist, amp, corr

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
      search_size = int(search_rad/arcsec_per_gp)+1
      sum_amp     = 0.0
      sum_amp_dist= 0.0
      do 100, i = search_posn-search_size, search_posn+search_size
          if (map(i).gt.min_amp) then
              sum_amp      = sum_amp + map(i)
              sum_amp_dist = sum_amp_dist + map(i)*real(i-search_posn)
          end if
  100 continue

      amp = sum_amp
      dist= arcsec_per_gp *
     *      (sum_amp_dist/sum_amp + real(search_posn-(map_size/2+1)))
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
          call ion_wrerr( s, ' in subroutine ion_calc_type2 ' )
          return
      end
