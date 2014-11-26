
C     *****************************************************************
C

C+lsf_set_model
C
      subroutine lsf_set_model( model_file,
     *                          interpolation_type,
     *                          noise_mean, noise_sigma,
     *                          s )

C     Sets up the non point source model in the lsf.
C
C     Given:
C         Model name - must be a non ambiguous aperture file name.
              character*(*)       model_file
C         Aperture interpolation type ( 1 = linear
C                                       2 = nonlinear - trad gaussian-sinc
C                                       3 = non-linear - fn. from map/ap
              integer             interpolation_type
C         noise mean and sigma
              real                noise_mean, noise_sigma

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     A status of ILL_MODEL is returned if the aperture is not regular
C     (i.e. is centrally situated in a power of 2 grid with zero skew)
C     Modified to read the phase reference of the aperture (ie RAOBS, DCOBS)
C                 and slide aperture across if non-linear interp. -conj
C
C     NPR     10 August 1988 + PJW 2/10/91 + 12/2/91
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i, j, index, c_index
C         File unit number
              integer         lun
C         Original redtape and projection parameter contents.
              integer         redtape(512), projection(32)
              integer         data_size
C         Aperture scaling
              real            uwl2gp, vwl2gp
              real*8          skew
C         Aperture phase reference and date of observation
              real*8          aper_ra, aper_dec, aper_date
C         Model aperture interp./degridding fn. specification and pars
              integer         fn_tp, fn_os, fn_hw
              real            fn_pa(2)

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

C     Establish and save what is in current redtape    suppressed PJW 26 2 92
      call dpredt( redtape, s )
      call dpproj( projection, s )

C     is this aperture file already in the lsf_model common block
C      if( model_file .eq. mod_ap_file ) then
C          write(1,*)' aperture already loaded'
C          goto 777
C      endif

C     Open model file, read redtape and map and enquire various things.
      call opemap( lun, model_file, 'READ', 0, s )
      call rdredt( lun, 1, 0, s )

      call enposobsdat( aper_ra, aper_dec, aper_date, s )
      mod_aperture_ra = aper_ra
      mod_aperture_dec = aper_dec
      mod_ap_interp = interpolation_type
      mod_noise_mean = noise_mean
      mod_noise_sigma = noise_sigma

      call enaper( uwl2gp, vwl2gp, skew, s )
      call enminirt( minirt, s )

      data_size = nx_mrt*ny_mrt
      if (dtp_mrt .eq. 4) data_size = 2*data_size

      if ( (s.eq.0) .and. (data_size.le.mod_max_data) ) then
C         Model definition OK.
          call rdmap( lun, 1, mod_aper, s )
          write(1, '(2a,/,a,4i5)' )
     *          '  reading aperture file: ',model_file(1:-1),
     *              '   size:',uv_mrt
C         if non-linear interp. and aperture origin on right-hand edge
C                             slide aperture data left 4
C                             and reset bits of minirt
          if( (interpolation_type .ge. 2) .and. (u2_mrt .eq. 0) ) then
              index = 1
              do i = v1_mrt, v2_mrt, -1
C                  if(nx_mrt.le.33)
C     *                write(1,'(10f12.1,/)')
C     *                   (mod_complex_aper(j),j=index,index+nx_mrt-1)
                  do j = u1_mrt+4, 0
                    mod_complex_aper(index) = mod_complex_aper(index+4)
                    index = index +1
                  enddo
                  index = index + 4
              enddo
              index = nx_mrt + nx_mrt - 3
              c_index = nx_mrt*ny_mrt - 5
              do i = v1_mrt-1, v2_mrt, -1
                  do j = 1,4
                      mod_complex_aper(index)
     *                     = conjg( mod_complex_aper(c_index) )
                      index = index + 1
                      c_index = c_index - 1
                  enddo
                  index = index + (nx_mrt-4)
                  c_index = c_index - (nx_mrt-4)
              enddo
              u1_mrt = u1_mrt + 4
              u2_mrt = u2_mrt + 4
C          else
C              write(1,*)'non-linear interp. with funny aperture',u2_mrt
          endif
          mod_u_wl2gp = uwl2gp
          mod_v_wl2gp = vwl2gp
          do  i = 1, minirt_len
              mod_minirt(i) = minirt(i)
          enddo

      else if (s.eq.0) then
          s = ILL_MODEL
      end if

      model_aperture = .true.
      mod_ap_file = model_file
      call enconv( fn_tp, fn_os, fn_hw, fn_pa, s )
      mod_ap_cspec(1) = fn_tp
      mod_ap_cspec(2) = fn_os
      mod_ap_cspec(3) = fn_hw
      mod_ap_cpa(1) = fn_pa(1)
      mod_ap_cpa(2) = fn_pa(2)

C     Restore redtape and close file
777   call ldredt( redtape, 0 )
      call ldproj( projection, 0 )
      close( lun )

      if (s .ne. 0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_MODEL' )
          return
      end
