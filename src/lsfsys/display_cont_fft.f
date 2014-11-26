C
C+display_cont_fft
C
      SUBROUTINE display_cont_fft(  lsf_num,
     *                              plot_device,
     *                              s                      )

C
C     Plots the Fourier transform of samples on the plot device.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Displays the fft's of a selected range of samples simultaneously.
C
C-
C=======================================================================
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/global_constants.inc'

C
C     Local constant and variable declarations
C
C     Constants
C         FFT dimensions.
              integer         fft_size
              parameter     ( fft_size = 2048 )
              integer         herm_fft_size
              parameter     ( herm_fft_size = fft_size/2 + 1 )

C     Variables, equivalences and commons
C         General purpose loop counters and character string
              integer         i, j
              character*80    string
C         Sample file logical unit number
              integer         sf_lun
C         Plot title
              character*(80)  title(4)
C         List of reals to plot
              real            plot_list( fft_size )
C         Spacing list, and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         Radii of each spacing and maximum aperture radius.
              real            radii( max_vis )
              real            max_radius
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         Index array used to average the visibility data.
              real            fft_index( herm_fft_size )
C         FFT array - complex before and real after
              complex         fft_array( herm_fft_size )
              real            post_fft_array( fft_size )
              equivalence   ( fft_array, post_fft_array )
C         Number of nonzero points in the fft
              real            fft_pts
C         The sampling, in wavelengths per gridpoint, for the fft
              real            wlen_p_gp
C         ... and arcsec per gridpoint after the fft
              real            arcsec_per_gp
C         The diplay radius in gridpoints and the plot centre
              integer         disp_rad, disp_posn
C         Number of points in the display and the min and max values.
              integer         num_pts
              real            max_plot
C         Increment and offset between sample plots
              real            increment, offset
C         An index for the current gridpoint
              real            gridpt
C         Number of visibilities to plot
              integer         num_spac
C         Current and last LSF buffer to display
              integer         buff_num, last_buff
C         Sampling rate in LSF buffers.
              integer         samp_rate
C         Flag to indicate whether first time through plot loop.
              logical         first_time

C    Place data in work space
              include '/mrao/post/include/post_work_array.inc'
              integer      i_point_1
              parameter   (i_point_1 = 1)
              equivalence (post_work(i_point_1), sp_list)
              integer      i_point_2
              parameter   (i_point_2 = i_point_1 + max_vis)
              equivalence (post_work(i_point_2), radii)
              integer      i_point_3
              parameter   (i_point_3 = i_point_2 + max_vis)
              equivalence (post_work(i_point_3), vis_list)
              integer      i_point_4
              parameter   (i_point_4 = i_point_3 + 2*max_vis)
              equivalence (post_work(i_point_4), plot_list)
              integer      i_point_5
              parameter   (i_point_5 = i_point_4 + fft_size)
              equivalence (post_work(i_point_5), fft_index)
              integer      i_point_6
              parameter   (i_point_6 = i_point_5 + herm_fft_size)
              equivalence (post_work(i_point_6), fft_array)



C Subroutine initialisation
C -------------------------

C     Check for non zero entry status
      if ( s .ne. 0 ) return
      first_time = .true.
      increment  = 0.0
      offset     = 0.0
      call lsf_enq_max_rad( lsf_num, max_radius, s )
      call lsf_enq_sf( lsf_num, sf_lun, i, s )

C     Get the sampling interval
      write(string, '(I4)' ) int(max_radius/100.0)
      call io_getr(  'Gridding interval (wavelengths/gridpoint) : ',
     *            string, wlen_p_gp, s )
      arcsec_per_gp = 1.0 /( const_sa2r * fft_size * wlen_p_gp )

C     Get display position and size in arcsec relative to map centre
      call io_geti('Display centre (arcsec from map-centre) : ',
     *          '0', disp_posn, s )

      disp_rad = int( 199 * arcsec_per_gp )
      write( string, '( I5 )' ) disp_rad
      call io_geti( 'Display radius (arcsec) : ', string, disp_rad, s )
      if ( s .ne. 0 ) goto 9999

C     Convert to gridpoints, with disp_posn being first gridpoint in
C     the display, and also reflected into the first positive cycle
C     of the fourier transform
      disp_rad = min0( int( disp_rad/arcsec_per_gp ), fft_size/2 )
      disp_posn = int( disp_posn/arcsec_per_gp ) - disp_rad
      disp_posn = mod( disp_posn, fft_size )
      num_pts   = 2*disp_rad + 1


C Main Code
C ---------

      call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
      call lsf_get_range( lsf_num, buff_num, last_buff, s )
      call io_geti( 'Sampling rate : ', '1', samp_rate, s )

      call plot_begin( plot_device, s )
      call pmadvance( s )

  100 if ((s .ne. 0) .or. (buff_num .gt. last_buff)) goto 1000
          call lsf_set_buffer(lsf_num,
     *                        buff_num,
     *                        s           )

          call lsf_get_vis(   lsf_num,
     *                        max_vis,
     *                        vis_list,
     *                        num_spac,
     *                        s           )

          call lsf_get_radii( lsf_num,
     *                        max_vis,
     *                        radii,
     *                        num_spac,
     *                        s           )
          if ( s .ne. 0 ) goto 9999

C         Initialise array
          do 200, i = 1, herm_fft_size
              fft_array( i ) = ( 0.0, 0.0 )
              fft_index( i ) = 0.0
  200     continue

          do 300, i = 1, num_spac
              if ( vis_list(i) .ne. (0.0, 0.0) ) then
                  gridpt = int( 0.5 + radii( i ) / wlen_p_gp )

C                 Ensure that oversampling is at least 10 percent
                  if ( int(gridpt*1.1) .le. herm_fft_size) then
                      fft_array(gridpt)= fft_array(gridpt) + vis_list(i)
                      fft_index(gridpt)= fft_index(gridpt) + 1.0
                  end if
              end if
  300     continue

C         Ensure reality of zero gridpoint
          fft_array(1) = cmplx( real(fft_array(1)), 0.0 )

C         Divide through, adding up the number of non zero points.
          fft_pts = 0.0
          do 400, i = 1, herm_fft_size
              if (fft_index(i) .ne. 0.0) then
                  fft_array( i ) = fft_array( i ) / fft_index( i )
                  fft_pts = fft_pts + 1.0
              end if
  400     continue

C         include the points from the other side of the fft
          if (fft_array(1) .eq. 0.0 ) then
              fft_pts = fft_pts+fft_pts
          else
              fft_pts = fft_pts+fft_pts-1
          end if

          if (fft_pts .gt. 0) then
              call hermitian_fft( fft_array, herm_fft_size, 1, s )

C             load into the plot array, normalising at the same time,
C             and initialise the index array
              max_plot = 0.0

              do 500, i = 1, num_pts
                  j = mod( disp_posn + fft_size + i-1, fft_size ) + 1
                  plot_list(i)=post_fft_array(j)*real(fft_size)/fft_pts
     *                         + offset
                  fft_index( i ) = (i-disp_rad-1) * arcsec_per_gp
                  max_plot = max0( plot_list( i ), max_plot )
  500         continue

C             Do plotting

              if ( first_time ) then
C                 Plot preliminaries
                  call pgvport( 0.1, 0.9, 0.85, 1.0 )
                  call pgwindow( 0.0, 100.0, 4.2, -0.2 )
                  call lsf_title( lsf_num, list, buff_num, last_buff,
     *                            title, s )

                  do 600, i = 1, 4
                      call pgtext( 0.0, real(i), title(i) )
  600             continue

                  call pgvport( 0.1, 0.9, 0.1, 0.75 )
                  call pgwindow( fft_index(1), fft_index(num_pts),
     *                           real(-4.0*max_plot), max_plot     )
                  call pgbox( 'BCNST', 0.0, 0, 'BCNST', 0.0, 0 )
                  call pglabel( 'Position (arcsec)', 'Flux (Jy)',
     *                      'FFT of instantaneous visibility' )
                  increment = -4.8 * max_plot *
     *                        samp_rate/(last_buff-buff_num+1)
              end if

C             Plot graph
              call pgline( num_pts, fft_index, plot_list )
              call pgpoint( 1, 0.0, offset, 2 )
              first_time = .false.
          end if

          buff_num = buff_num + samp_rate
          offset = offset + increment
      goto 100
 1000 continue

      call pgend
      if ( s .ne. 0 ) goto 9999

      return


C Error Handling
C --------------

 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine DISPLAY_CONT_FFT' )
          end if
          return
      end
