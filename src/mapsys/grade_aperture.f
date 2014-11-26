
C+grade_aperture
C
      SUBROUTINE grade_aperture(  aperture,
     *                            n1, n2,
     *                            u_wl2gp, v_wl2gp,
     *                            grad_type, grad_param,
     *                            beam, grad_flag,
     *                            s                        )

C
C     Applies the grading function and also scales the aperture.
C
C     Given:
C         Aperture bounds - n1 is the number of columns, n2 is rows.
              integer         n1, n2
C         Aperture array - element (n1, n2/2+1) is point (0,0)
              complex         aperture( n1,n2 )
C         Conversion factor from wavelengths to gridpoints in u and v.
              real            u_wl2gp, v_wl2gp
C         Grading function type.
              integer         grad_type
C         Grading function parameters.
              real            grad_param(*)
C         The aperture of the beam.
              real            beam( n1,n2 )
C         Function type flag (for valid values, see below)
              integer         grad_flag

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     This routine applies or removes grading on the basis of the value
C     of the parameter grad_flag. If:
C
C         grad_flag = 1 - grading applied to both aperture and beam.
C         grad_flag = 2 - grading removed from aperture only.
C         grad_flag = 3 - grading removed from both aperture and beam.
C         otherwise     - grading applied to aperture only.
C
C     For grading flags 1 and 3, both the beam and aperture are
C     renormalised so that the height of the beam in the map plane is
C     unity. For other grading flags the beam is not used and so can
C     be input as a dummy variable.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/maplib_redtape.inc'
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/include/maplib_tabfns.inc'
      include        '/mrao/post/include/grading_types.inc'

C     ****************************************************************
C
C     Local parameter declaration.
C
C         Maximum number of points in function tabulation
              integer         max_pts
              parameter     ( max_pts = 1200 )


C     Local variable declarations
C         Loop counters
              integer         i, j
C         Grading function tabulation and parameters
              integer         fn_type, fn_hw, fn_os
              real            fn_param(2), fn_tab( 0:max_pts )
C         Grading to apply for a given element.
              real            grad
C         Twice the variance of the gaussian grading
              real            var_by2
C         Conversion factor from aperture wavelengths to table index
              real            wl2tab
C         Pointers to table
              integer         tab_iptr
              real            tab_ptr, frac
C         The sum of the grading function points.
              real*8          beam_sum
C         The aperture normalisation factor.
              real            aper_norm
C         The radius squared of the gridpoint in the aperture plane.
              real            radius_sqr
C         Row and column in the aperture of the (0,0) point
              integer         zero_row, zero_col
C         U and V coordinates, in wavelengths of the projection of the
C         current gridpoint onto the equatorial plane.
              real            u, v
C         Conversion factor from gridpoints to wavelengths.
              real            u_gp2wl, v_gp2wl
C         Round beam control switch
              logical         round_beam

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Check if round beam
      if ( grad_type.ge.100 ) then
          round_beam = .true.
          grad_type = grad_type - 100
          v_wl2gp = real(iymax) * usamp * const_sa2r * dsin( decobs )
      else
          round_beam = .false.
      endif

C     Initialise and check the various grading types.
      if (grad_type.eq.gaussian_gr.or.grad_type.eq.radial_gauss_gr) then
          var_by2   = 2 * grad_param(1) * grad_param(1) *
     *                real((n1-1)*(n1-1))/(u_wl2gp*u_wl2gp)
      else if (grad_type.eq.psw2_gr .or. grad_type.eq.l2w2_gr .or.
     *         grad_type.eq.psw3_gr .or. grad_type.eq.l2w3_gr .or.
     *         grad_type.eq.optimal_gr                            ) then
C         Tabulated grading function - set up the table
          if (grad_type.eq.psw2_gr) then
              fn_type = prol_spher
              fn_hw   = 2
          else if (grad_type.eq.psw3_gr) then
              fn_type = prol_spher
              fn_hw   = 3
          else if (grad_type.eq.l2w2_gr) then
              fn_type = l2_optimal
              fn_hw   = 2
          else if (grad_type.eq.l2w3_gr) then
              fn_type = l2_optimal
              fn_hw   = 3
          else if (grad_type.eq.optimal_gr) then
              fn_type = opt_grading
              fn_hw   = int(1.0/grad_param(1))+1
          end if

          fn_os       = max_pts/fn_hw
          fn_param(1) = grad_param(2)
          wl2tab      = fn_os / abs(grad_param(1)*(n1-1)/u_wl2gp)
          if (grad_type.ne.optimal_gr) wl2tab = wl2tab*fn_hw

          call tabfn(-fn_type,fn_os,fn_hw,fn_param, max_pts,fn_tab, s )
      else if (grad_type.ne.radial_gr.and.grad_type.ne.no_grading) then
          s = ILL_GRADING
      end if
      if (s.ne.0) goto 9999

C     ****************************************************************
C
C         Main Code
C         ---------
C
      zero_col = n1
      zero_row = n2/2+1
      beam_sum = 0.0D+0
      u_gp2wl  = 1.0/u_wl2gp
      v_gp2wl  = 1.0/v_wl2gp

      do 200, j = 1, n2
          v = real(zero_row-j)*v_gp2wl
          do 100, i = 1, n1
              if (aperture(i,j) .ne. (0.0,0.0)) then
                  u = real(zero_col-i)*u_gp2wl
                  if (grad_type .eq. gaussian_gr ) then
                      grad = exp(-(u*u+v*v)/var_by2)
                  else if ( grad_type .eq. radial_gr ) then
                      grad = sqrt(u*u+v*v)
                  else if (grad_type .eq. radial_gauss_gr ) then
                      radius_sqr= u*u+v*v
                      grad = sqrt(radius_sqr)*exp(-radius_sqr/var_by2)
                  else if (grad_type .eq. no_grading) then
                      grad = 1.0
                  else
                      tab_ptr = wl2tab*sqrt(u*u+v*v)
                      tab_iptr= int(tab_ptr)
                      if (tab_iptr.ge.(fn_hw*fn_os)) then
C                         Off table
                          grad = 0.0
                      else
                          frac = tab_ptr-tab_iptr
                          grad = (1.0-frac)*fn_tab(tab_iptr) +
     *                                frac *fn_tab(tab_iptr+1)
                      end if
                  end if

                  if (grad_flag.eq.2.or.grad_flag.eq.3) then
C                     Remove grading rather than apply it.
                      if (grad.ne.0.0) grad = 1.0/grad
                  end if

                  if (grad_flag.eq.1.or.grad_flag.eq.3) then
C                     Apply grading to aperture and beam
                      aperture(i,j) = aperture(i,j) * cmplx(grad,0.0)
                      beam(i,j)     = beam(i,j) * grad
                      beam_sum      = beam_sum + 2*beam(i,j)
                  else
                      aperture(i,j) = aperture(i,j) * cmplx(grad,0.0)
                  end if
              else
C                 Aperture is null - ensure beam is too.
                  if (grad_flag.eq.1.or.grad_flag.eq.3) beam(i,j)=0.0
              end if
  100     continue

C         Remove doubled up final column from beam sum.
          if (grad_flag.eq.1.or.grad_flag.eq.3) then
              beam_sum = beam_sum - beam(n1,j)
          end if
  200 continue

C     Normalise aperture and beam, if neccesary.
      if (grad_flag.eq.1.or.grad_flag.eq.3) then
          aper_norm = real(2*(n1-1)*n2) / real(beam_sum)

          do 400, j = 1, n2
              do 300, i = 1, n1
                  aperture(i,j) = aperture(i,j)*cmplx(aper_norm,0.0)
                  beam(i,j)     = beam(i,j)*aper_norm
  300         continue
  400     continue
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine GRADE_APERTURE' )
          return
      end
