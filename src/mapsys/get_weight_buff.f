C
C+get_weight_buff
C
      SUBROUTINE get_weight_buff( num_uv,
     *                            vis, uv,
     *                            weight_type, weight_param,
     *                            smooth_arr,
     *                            n1, n2,
     *                            weight,
     *                            s                  )

C
C     Calculates the weights to be given to a buffer of uv positions.
C
C     Given:
C         The number of uv points.
              integer             num_uv
C         The visibility buffer
              complex             vis( num_uv )
C         The uv coordinate buffer.
              real                uv( 2, num_uv )
C         weighting type and smooth type.
              integer             weight_type
C         weighting function parameters
              real                weight_param(*)
C         Dimensions of the half plane aperture.
              integer             n1, n2
C         The super smooth weighting array - (0,0) is at (n1,n2/2+1)
              real                smooth_arr( n1,n2 )
C
C     Returned:
C         The array of weights returned.
              real                weight( num_uv )
C         Status value - must be zero on entry.
              integer             s
C
C     Generates a weighting array given a buffer of uv points and the
C     smoothing array and weighting type and parameters. The uv array
C     must be expressed in units of aperture gridpoints and the
C     weighting is calculated using the following weighting types
C     defined in the file (postmortem)weighting-types:incl :
C
C     0     - No weighting.
C     1     - Super-smooth weighting :    Weight is from weight array.
C     2     - Radial weighting :          w = r
C     3     - Gaussian weighting :        w = e**( -r**2/sigma**2 )
C             weight_param(1) = sigma
C     4     - Radial*Gaussian weighting   w = r * e**( -r**2/sigma**2 )
C             weight_param(1) = sigma
C     10-15 - Same as above except a cutoff is applied at a radius of
C             weight_param(2).
C
C     Note that sigma and the cutoff refer to the aperture plane, not
C     the equatorial plane (they are different for sky coordinates).
C     They are both expressed as a fraction of the aperture plane
C     halfwidth measured along the axis.
C
C     NPR     8 December 1987.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/post/include/weighting_types.inc'

C     ****************************************************************
C
C     Variables, equivilances and commons
C         Loop counter
              integer         i
C         Twice the variance of the gaussian weighting.
              real            var_by2
C         Square of the radius of the current gridpoint.
              real            radius_sqr
C         Weighting type disregarding any cutoff weighting.
              integer         prim_wt_type
C         Weight to be given to the current sample - as R*8 and R*4
              real*8          r8buff_wt
              real            buff_wt
C         Maximum radius before cutoff squared.
              real            max_r_sqr
C         The nearest integral u, v coordinate
              integer         u, v
C         The row and column of the (0,0) point in the aperture
              integer         zero_row, zero_col

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if (weight_type .lt. 10) then
          prim_wt_type = weight_type
      else
          prim_wt_type = mod(weight_type,10)
      end if

      if ( prim_wt_type .eq. no_weighting ) then
          do 100, i = 1, num_uv
              weight(i) = 1.0
  100     continue
      else if ( prim_wt_type .eq. super_smooth ) then
          zero_col = n1
          zero_row = n2/2+1
          do 200 i = 1, num_uv
              u = nint(uv( 1, i ))
              v = nint(uv( 2, i ))
              if ( u .gt. 0 ) then
                  weight(i) = smooth_arr(zero_col-u,zero_row+v)
              else
                  weight(i) = smooth_arr(zero_col+u,zero_row-v)
              end if
              weight(i) = weight(i)
  200     continue
      else if ( prim_wt_type .eq. radial_wt ) then
          do 300, i = 1, num_uv
              weight(i) = sqrt(uv(1,i)*uv(1,i) + uv(2,i)*uv(2,i))
  300     continue
      else if (prim_wt_type .eq. gaussian_wt ) then
          var_by2=weight_param(1)*weight_param(1)*real(2*(n1-1)*(n1-1))
          do 400, i = 1, num_uv
              radius_sqr= uv(1,i)*uv(1,i)+uv(2,i)*uv(2,i)
              weight(i) = exp(-radius_sqr/var_by2)
  400     continue
      else if (prim_wt_type .eq. radial_gauss_wt ) then
          var_by2=weight_param(1)*weight_param(1)*real(2*(n1-1)*(n1-1))
          do 500, i = 1, num_uv
              radius_sqr= uv(1,i)*uv(1,i)+uv(2,i)*uv(2,i)
              weight(i) = sqrt(radius_sqr)*exp(-radius_sqr/var_by2)
  500     continue
      else if ( prim_wt_type .eq. noise_wt ) then
          call buffer_wt( num_uv, vis, r8buff_wt )
          buff_wt  = r8buff_wt
          zero_col = n1
          zero_row = n2/2+1
          do 600 i = 1, num_uv
              u = nint(uv( 1, i ))
              v = nint(uv( 2, i ))
              if ( u .gt. 0 ) then
                  weight(i) = smooth_arr(zero_col-u,zero_row+v)
              else
                  weight(i) = smooth_arr(zero_col+u,zero_row-v)
              end if
              weight(i) = weight(i)*buff_wt
  600     continue
      else
          S = ILL_WEIGHT
          goto 9999
      end if

      if (weight_type .ne. prim_wt_type) then
C         Apply cutoff
          max_r_sqr=weight_param(2)*weight_param(2)*real((n1-1)*(n1-1))
          do 700, i = 1, num_uv
              if ((uv(1,i)*uv(1,i)+uv(2,i)*uv(2,i)).gt.max_r_sqr) then
                  weight(i) = 0.0
              end if
  700     continue
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine GET_WEIGHT_BUFF' )
          return
      end
