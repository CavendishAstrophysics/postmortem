C
C+ion_calc_type3
C
      SUBROUTINE ion_calc_type3( map, search_posn, ion, s )

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
              integer         i, j
C         Radius and boundary of search
              integer         search_size, search_bdry
C         Centre of map and of search in pixels
              integer         map_cent, centre
C         Start and finish of search in pixels
              integer         start, finish
C         Calculation variables.
              real            sum_amp, sum_amp_dist, dist, amp, corr
              real            map_max, cutoff

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
C     Find flux centroid.
      map_cent    = map_size/2+1
      search_bdry = int(search_rad/arcsec_per_gp)+1
      search_size = int(source_size/arcsec_per_gp)+1

      map_max     = 0.0
      do 100, i = max( search_posn-search_bdry, 1 ),
     *            min( search_posn+search_bdry, map_size )
          if (map(i).gt.map_max) then
              map_max = map(i)
              centre  = i
          end if
  100 continue
      cutoff = map_max*min_amp

      do 300, i = 1, num_iters
          sum_amp     = 0.0
          sum_amp_dist= 0.0
          start  = max( centre-search_size, search_posn-search_bdry,
     *                  1                                           )
          finish = min( centre+search_size, search_posn+search_bdry,
     *                  map_size                                    )

          do 200, j = start, finish
              if (map(j).gt.cutoff) then
                  sum_amp      = sum_amp + map(j)
                  sum_amp_dist = sum_amp_dist + map(j)*real(j-centre)
              end if
  200     continue

          if (sum_amp .ne. 0.0) then
              dist = sum_amp_dist/sum_amp + real(centre-map_cent)
          else
              dist = 0.0
          end if
          centre = map_cent + nint( dist )
  300 continue

      amp = sum_amp
      corr= sin(dist*arcsec_per_gp*const_sa2r)*const_2pi

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
          call ion_wrerr( s, ' in subroutine ion_calc_type3 ' )
          return
      end
