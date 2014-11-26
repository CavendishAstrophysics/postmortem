C
C+ion_change_params
C
      SUBROUTINE ion_change_params( sf_name, s )

C
C     Asks user for changes to the parameters in the correction defn.
C
C     Given:
C         Physical sample file name
              character*(*)       sf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function Declarations
C
      include        '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Loop counter
              integer             i
C         Logical sample file number
              integer             lsf_num
C         Minimum and maximum sampling limits
              real                min_samp, max_samp
C         The maximum radius of the aperture plane
              real                max_radius
C         Buffer holding old parameters
              integer             old_param( max_ionpars ), old_type

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      if (ion_type .eq. 0) then
          call io_wrout(
     *         'You cannot change parameters of an old correction.')
          return
      else
          old_type = ion_type
          do 10, i = 1, max_ionpars
              old_param(i) = ion_param(i)
   10     continue
      end if

      call lsf_open( sf_name, ion_lsf, 'READ', lsf_num, s )
      call lsf_enq_max_rad( lsf_num, max_radius, s )
      call lsf_close( lsf_num, s )
      if ( s .ne. 0 ) goto 9999

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call io_geti('1-D map size: ', '*', map_size, s )
      call io_getr('Sampling in arcsec/gridpoint: ', '*',
     *                                                arcsec_per_gp, s )
      call io_getr('Search radius in arcseconds: ', '*', search_rad, s )
      if (io_yesno('Find position from flux centroid ?', 'No', s )) then
          call io_getr(
     *       'Flux minimum (expressed as a ratio of the peak height): ',
     *        '*', min_amp, s )
          if (io_yesno( 'Iterate to a solution ?', 'Yes', s )) then
            ion_type = 3
            call io_getr( 'Maximum source radius in arcsec: ', '*',
     *                    source_size, s )
            call io_geti( 'Number of iterations: ', '*', num_iters, s )
          else
            ion_type = 2
          end if
      else
          ion_type = 1
      end if
      call io_getr('Additional radial factor (0. implies none): ',
     *          '*', radial_factor, s )

      if ( s .ne. 0 ) goto 9999

C     Adjust values to reasonable numbers.
c     map_size = nint(2.0**real(nint(alog2(real(map_size)))))
      map_size = nint(2.0**real(nint(alog(real(map_size))/alog(2.0))))
      map_size = max( 32, min( map_size, max_map_size ))

      max_samp = 1.0 /( 1.2 * const_sa2r * 2.0 * max_radius )
      min_samp = 1.0 /( 100.0 * const_sa2r * 2.0 * max_radius )
      arcsec_per_gp = min( max_samp, max( min_samp, arcsec_per_gp ))

      search_rad = max( 8.0*arcsec_per_gp,
     *             min( search_rad,real(map_size/2-1)*arcsec_per_gp))

      min_amp    = max( 0.0, min( min_amp, 1.0 ) )
      source_size= max(8.0*arcsec_per_gp,min(search_rad,source_size))
      num_iters  = max( 1, min( num_iters, 10 ))
      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_change_params ' )
          else
              ion_type = old_type
              do 20, i = 1, max_ionpars
                  ion_param(i) = old_param(i)
   20         continue
          end if
          return
      end
