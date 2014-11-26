
C
C+ion_transform
C
      SUBROUTINE ion_transform(   lsf_num,
     *                            buff_num,
     *                            aperture,
     *                            start_map, end_map,
     *                            s                      )

C
C     Does the ion correction transform for a given buffer number
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Buffer number to do the transform on
              integer             buff_num
C         Aperture to do the transform in
              complex             aperture(*)
C         First pixel and last pixel of interest in the resultant map.
              integer             start_map, end_map
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Constructs a 1-D map for a given lsf buffer so that an
C     an ionospheric correction can be calculated from it.
C
C     If the buffer number is negative then an ideal beam will be made
C     for the buffer with the same absolute value as the buffer number.
C
C     A status of NO_VISDATA is returned if the data buffer is null but
C     no error is logged.
C
C     Additional radial weighting ( if radial_factor .ne. 0. )
C     Weight = radius * radial_factor                       pjw 10/5/89
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/ionsys_errors.inc'
      include  '/mrao/post/include/ion_runtime.inc'
      include  '/mrao/post/include/clfst_constants.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         General purpose loop counters and character string
              integer         i
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_spac )
C         Radii of each spacing and the maximum aperture radius.
              real            radii( max_spac )
C         Current source number.
              integer         src_num
C         The aperture size in gridpoints.
              integer         aper_size
              real            aper_radius
C         Number of visibilities in the buffer.
              integer         num_spac
C         Number of non-zero points in the aperture
              real            aper_pts
C         A weighting array for the aperture.
              integer         aper_wt( max_aper_size )
C         The sampling, in wavelengths per gridpoint, for the aperture
              real            gp_p_wlen
C         An index for the current gridpoint
              real            gridpt
C         The map correction scaling factor
              integer         map_corr
C         Radial weighting control and factor
              logical         radial_wt
              real            wt
C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      aper_size   = int(map_size/2)+1
      aper_radius = real(aper_size)+0.5
      gp_p_wlen   = real(map_size) * arcsec_per_gp * const_sa2r
      if( radial_factor .ne. 0. ) then
          radial_wt = .true.
      else
          radial_wt = .false.
      endif

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call lsf_set_buffer(lsf_num, iabs(buff_num), s )
      call lsf_get_vis(   lsf_num,
     *                    max_spac,
     *                    vis_list,
     *                    num_spac,
     *                    s           )

C     Initialise array
      do 100, i = 1, aper_size
          aperture( i ) = ( 0.0, 0.0 )
          aper_wt( i )  = 0
  100 continue

      do 300, src_num = 1, ion_numsrcs
          if (buff_num .le. 0) then
C             Construct ideal beam.
              do 150, i = 1, num_spac
                  vis_list(i) = (1.0,0.0)
  150         continue
          else if (src_num .gt. 1) then
C             Rotate to source (buffer is phased to first source).
              call lsf_rot_vis( lsf_num, num_spac, vis_list,
     *                          ion_ra(src_num), ion_dec(src_num), s )
          end if

          call lsf_get_radii( lsf_num, max_spac, radii, num_spac, s )
          if ( s .ne. 0 ) goto 9999

          wt = 1.
          do 200, i = 1, num_spac
              if ( vis_list(i) .ne. (0.0, 0.0) ) then
                  if( radial_wt ) wt = radii(i)*radial_factor
                  gridpt = int( aper_radius-radii(i)*gp_p_wlen )
                  aperture(gridpt) = aperture(gridpt) + vis_list(i) * wt
                  aper_wt(gridpt)  = aper_wt(gridpt) + 1
              end if
  200     continue
  300 continue

C     Ensure reality of zero gridpoint
      aperture(aper_size)
     *            = cmplx(real(aperture(aper_size)), 0.0)

C     Divide through, finding the number of non zero points.
C     and inverting alternate pixels to rotate fft.
      aper_pts = 0
      do 400, i = 1, aper_size
          if (aper_wt(i) .ne. 0) then
              aperture(i) = aperture(i) / real(aper_wt(i))
              if (mod(i,2).eq.0) aperture(i)=-aperture(i)
              aper_pts = aper_pts + 1
          end if
  400 continue

C     Include the points from the other side of the fft
      if (aperture(aper_size) .eq. (0.0,0.0)) then
          aper_pts = aper_pts+aper_pts
      else
          aper_pts = aper_pts+aper_pts-1
      end if

      if (aper_pts .eq. 0) then
          s = NO_VISDATA
          goto 9999
      end if

C     Do the fft.
      call hermitian_fft( aperture, aper_size, 1, s )

C     Weight the array and invert alternate pixels to complete rotation.
      map_corr = real(ion_numsrcs)*real(map_size)/real(aper_pts)
      do 500, i = int((start_map+1)/2), int(end_map/2)+1
          aperture(i) = conjg(aperture(i))*cmplx(map_corr,0.0)
  500 continue

      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. NO_VISDATA) then
              call ion_wrerr( s, ' in subroutine ion_transform ' )
          end if
          return
      end
