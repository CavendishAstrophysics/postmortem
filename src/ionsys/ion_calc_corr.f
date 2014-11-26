C
C+ion_calc_corr
C
      SUBROUTINE ion_calc_corr( map, search_posn, ion, s )

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
      include  '/mrao/post/include/ionsys_errors.inc'
      include  '/mrao/post/include/clfst_constants.inc'
      include  '/mrao/post/include/ion_runtime.inc'

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
      if (ion_type .eq. 1) then
          call ion_calc_type1( map, search_posn, ion, s )
      else if (ion_type .eq. 2) then
          call ion_calc_type2( map, search_posn, ion, s )
      else if (ion_type .eq. 3) then
          call ion_calc_type3( map, search_posn, ion, s )
      else
          s = ILL_ION
          goto 9999
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_calc_corr ' )
          return
      end
