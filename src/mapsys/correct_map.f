C
C+correct_map
C
      SUBROUTINE correct_map( map,
     *                        n1, n2,
     *                        conv_hw, conv_os, max_conv_pts, conv,
     *                        corr_type,
     *                        invert_flag,
     *                        s                        )

C
C     Applies or removes the gridding correction function.
C
C     Given:
C         Map bounds - n1 is the number of columns, n2 is rows.
              integer         n1, n2
C         Map array
              real            map( n1,n2 )
C         Convolution function array halfwidth (for dimensioning)
              integer         max_conv_pts
C         Actual conv fn. halfwidth and over sampling.
              integer         conv_hw, conv_os
C         Tabulated convolution function.
              real            conv( -max_conv_pts:max_conv_pts )
C         Correction function type.
              integer         corr_type
C         Invert flag - set true to divide by the gridding correction
C                       function, instead of multipying by it.
C                       (ie set when converting a map into an aperture.)
              logical         invert_flag

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Multiplys the map by the gridding correction function for a given
C     convolution function.  The inverse of this process can be done if
C     the parameter invert_flag is set .true., instead of .false., which
C     is normally the case.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/maplib_errors.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         Maximum size of correction array
              integer         max_corr_size
              parameter     ( max_corr_size = 1024 )

C     Variables, equivilances and commons
C         Loop counters
              integer         i, j
C         Correction array
              real            corr( max_corr_size )

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ((n1 .gt. max_corr_size) .or. (n2 .gt. max_corr_size)) then
          s = ARR_TOOSMALL
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call corrfn( conv_hw, conv_os, max_conv_pts, conv,
     *             corr_type, n1, corr, s                )
      if ( s .ne. 0 ) goto 9999

      if ( n1 .eq. n2 ) then
          if (.not. invert_flag) then
              do 200, i = 1, n1
                  do 100, j = 1, n2
                      map(j,i) = map(j,i)*corr(i)*corr(j)
  100             continue
  200         continue
          else
              do 400, i = 1, n1
                  do 300, j = 1, n2
                      map(j,i) = map(j,i)/(corr(i)*corr(j))
  300             continue
  400         continue
          end if
      else
          do 600, j = 1, n2
              do 500, i = 1, n1
                  if (invert_flag) then
                      map( i, j ) = map(i,j)/corr(i)
                  else
                      map( i, j ) = map(i,j)*corr(i)
                  end if
  500         continue
  600     continue

          call corrfn( conv_hw, conv_os, max_conv_pts, conv,
     *                 corr_type, n2, corr, s               )
          if ( s .ne. 0 ) goto 9999

          do 800, j = 1, n2
              if (invert_flag) then
                  corr(j) = 1.0/corr(j)
              else
                  corr(j) = corr(j)
              end if

              do 700, i = 1, n1
                  map( i, j ) = map( i, j )*corr( j )
  700         continue
  800     continue
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine CORRECT_MAP' )
          return
      end
