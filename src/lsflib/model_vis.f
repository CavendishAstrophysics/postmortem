
C     *****************************************************************
C
C+model_vis
C
      subroutine model_vis( model_type, model_flux, model_ra, model_dec,
     *                      mod_vis, s )

C     Returns current model visibilities for the lsf.
C         and the actual position of the sample for a model.
C
C     Given:
C         Model type indicator - 1 if point source, otherwise aperture
C                                  with linear or non-linear interp.
              integer         model_type
C         Flux of source
              real            model_flux

C     Returned:
C         RA and DEC - current  position for point model
C                      actual position from redtape for aperture model
              real*8          model_ra, model_dec
C         Buffer of model visibilities
              complex         mod_vis( * )
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_model.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/include/maplib_errors.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         U and V position in model
              real*8          uv(2)
C         Flag set if in positive-U half plane of the aperture
              logical         conjg_flg
C         Number of visibilities returned from lsf_get_uv
              integer         num_vis
C         Interpolation type 1 = linear
C                            2 = non-linear - trad. gauss-sinc
C                            3 = non-linear with actual fn. used in map
              integer         interp_type
C         noise mean and sigma
              real            mean, sigma

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
      if ( model_type.eq.1 ) then
          do 100, i = 1, sp_list_len
              mod_vis( i ) = cmplx( model_flux, 0.0 )
  100     continue
          num_vis = sp_list_len
      else
C         Get the model from the stored aperture definition
C         Use the visibility buffer as temporary storage for uv's.
          call lsf_get_uv( curr_lsf_num, max_rt_sp,
     *                     0.0D+0, mod_vis, num_vis, s )

          interp_type = mod_ap_interp

          do 200, i = 1, num_vis
              uv(1) = real(mod_vis(i))*mod_u_wl2gp
              uv(2) = imag(mod_vis(i))*mod_v_wl2gp
C              conjg_flg = (uv(1).ge.mod_minirt(u2_ptr))
              conjg_flg = ( uv(1) .ge. 0.0d0 )
              if ( conjg_flg ) then
                  uv(1) = -uv(1)
                  uv(2) = -uv(2)
              end if
              call degrid( mod_minirt, mod_aper, uv, interp_type,
     *                     mod_ap_cspec, mod_ap_cpa, mod_vis(i), s )
              if (s .eq. -25) then
C         zero-zero returned for off aperture points
C                 write(1,*)'u or v probably outside model aperture',uv
C                  goto 9999
                  mod_vis(i) = ( 0., 0. )
                  s = 0
              elseif( s .ne. 0 ) then
                  goto 9999
              endif

              if (mod_minirt(dtp_ptr) .eq. 4) then
                  if (conjg_flg) mod_vis(i) = conjg( mod_vis(i) )
              else
                  mod_vis( i ) = cmplx( real(mod_vis(i)) )
              end if
              mod_vis(i) = mod_vis(i)*cmplx( model_flux, 0.0 )
  200     continue

          model_ra = mod_aperture_ra
          model_dec = mod_aperture_dec
      end if

C    "add" noise if required

      mean = mod_noise_mean
      sigma = mod_noise_sigma
      call noise( num_vis, mod_vis, mean, sigma, 2, s )
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine MODEL_VIS' )
          return
      end
