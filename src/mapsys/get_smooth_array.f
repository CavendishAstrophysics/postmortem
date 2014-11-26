C
C+get_smooth_array
C
      SUBROUTINE get_smooth_array(    smooth_arr,
     *                                n1, n2,
     *                                uwl2gp, vwl2gp,
     *                                weight_type, weight_params,
     *                                work,
     *                                s                  )

C
C     Returns the aperture plane super-smooth weighting array.
C
C     Given:
C         Aperture size.
              integer             n1, n2
C         The number of gridpoints per wavelength in u and v.
              real                uwl2gp, vwl2gp
C         Smooth type
              integer             weight_type
C         Smooth parameters
              real                weight_params(*)
C         Work space - must be twice the size of the smoothing array
              real*8              work( n1, n2 )
C
C     Returned:
C         The super-smooth weighting array - (0,0) is at (n1,n2/2+1)
              real                smooth_arr( n1, n2 )
C         Status value - must be zero on entry.
              integer             s
C
C     Generates a super-smooth weighting array for use with the
C     visibility data and map information defined in the current map
C     redtape.
C
C     The weighting array has the same number of elements as the
C     aperture array but is ordered with the direction of the second
C     index in the opposite direction to the conventional arrangement
C     for an aperture. Thus, the value of v increases as you move
C     forwards through the array, that being the conventional FORTRAN
C     way. The weight to be given to a particular visibility point is
C     given by the value of nearest pixel in the weighting array.
C
C     The weighting types are given in (postmortem)weighting-types:incl.
C
C     NPR.    7 November 1987.
C
C-
C     ****************************************************************
C
C     Function declarations -
C
      include        '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/maplib_redtape.inc'
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/post/include/global_constants.inc'
      include        '/mrao/post/include/weighting_types.inc'

C     ****************************************************************
C
C     Variables, equivilances and commons
C         Loop counters
              integer         i, j, k
C         Integral values of u and v
              integer         iu, iv
C         The row and column of the (0,0) element
              integer         zero_row, zero_col
C         Rows and columns
              integer         row, col, last_col
C         Current element in weight array
              real            curr_element
C         Flag indicating that convolution has run off the aperture
              logical         fold
C         Full sample file name and lsf key.
              character*(80)  full_sf_name
              integer         lsf_key
C         Current logical sample file number
              integer         lsf_num
C         Number of buffers in the current lsf.
              integer         num_buff
C         Number of visibilities in the current buffer
              integer         num_vis
C         Visibility buffer.
              complex         vis( max_vis )
C         UV coordinate buffer
              real            uv( 2, max_vis )
C         Weight to be given to a particular buffer
              real*8          buff_wt
C         Logical set for noise weighting, unset for super-smooth
              logical         noise_weighting
C         The effective skew angle of the map.
              real*8          skew_angle
C         The radius, in gridpoints, to smooth over.
              integer         smooth_radius
C         String and length
              character*47    string
              integer         ls
C         Execution mode information
              character*16    current_user
              integer         exe_mode, termno

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

      if ( mod(wghttp,10) .eq. super_smooth ) then
          noise_weighting = .false.
          buff_wt         = 1.0D+0
      else if ( mod(wghttp,10) .eq. noise_wt ) then
          noise_weighting = .true.
      else
C         Not super smooth or noise weighting - array left undefined
          return
      end if

C     Find execution mode
      call io_enqexe( current_user, exe_mode, termno )

C     Find smooth radius in pixels.
      smooth_radius = nint( weight_params( 1 ) )

C     Initialise arrays
      do 40, i = 1, n2
          do 30, j = 1, n1
              work( j,i )       = 0.0D+0
              smooth_arr( j,i ) = 0.0
   30     continue
   40 continue

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Read in the visibility points and sum to the nearest grid point
      zero_col = n1
      zero_row = n2/2+1

      string(1:)    = 'Smoothing'

      do 1000, i = 1, numlsf
          call enmlsf( i, full_sf_name, lsf_key, s )
          string(11:26) = full_sf_name
          string(28:47) = '>-------------------'
          if (exe_mode.eq.0) then
            call io_wrout( string )
          else
            call io_wrout( string(1:27) )
          end if
          call lsf_open( full_sf_name, lsf_key, 'READ', lsf_num, s )
          call lsf_enq_numbuff( lsf_num, num_buff, s )

C         Find the effective UV plane skew angle for this LSF
C         - this also sets the phase centre of the LSF
          call get_skew_angle( lsf_num, skew_angle, s )
          if ( s .ne. 0 ) goto 9999

          do 500, j = 1, num_buff
              if ( mod(j, max0(1,int(num_buff/20))) .eq. 0 ) then
                  ls = chr_lend( string, '>' )
                  string(ls+1:ls+2) = '->'
                  if (exe_mode.eq.0) write(1,'(''+'',A)') string
              end if

              call lsf_set_buffer( lsf_num, j, s )
              call lsf_get_vis(   lsf_num,
     *                            max_vis,
     *                            vis,
     *                            num_vis,
     *                            s          )
              if (noise_weighting) then
                  call buffer_wt( num_vis, vis, buff_wt )
              end if

              call lsf_get_uv(    lsf_num,
     *                            max_vis,
     *                            skew_angle,
     *                            uv,
     *                            num_vis,
     *                            s          )
              if ( s .ne. 0 ) goto 9999

              do 400 k = 1, num_vis
                  if ( vis( k ) .ne. ( 0.0, 0.0 ) ) then
                      iu = nint( uv( 1, k ) * uwl2gp )
                      iv = nint( uv( 2, k ) * vwl2gp )
                      if ( iu .gt. 0 ) then
                          col = zero_col-iu
                          row = zero_row+iv
                      else
                          col = zero_col+iu
                          row = zero_row-iv
                      end if
                      work( col,row ) = work( col, row ) + buff_wt
                  end if
  400         continue
  500     continue

          call lsf_close( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
 1000 continue

C     Now convolve with a boxcar...
      do 160 i = 1+smooth_radius, n2-smooth_radius
          do 150 j = 1+smooth_radius, n1
              curr_element = work(j,i)

              if (curr_element .ne. 0) then
                  last_col = j+smooth_radius
                  fold     = (last_col .ge. n1)
                  if (fold) last_col = n1

                  do 110 row = i-smooth_radius, i+smooth_radius
                      do 120, col = j-smooth_radius, last_col
                          smooth_arr(col,row) =
     *                                smooth_arr(col,row)+curr_element
  120                 continue
  110             continue

                  if (fold) then
                      do 140 row = (n2+2-i)-smooth_radius,
     *                             (n2+2-i)+smooth_radius
                          do 130, col = (n1+n1)-(j+smooth_radius), n1
                              smooth_arr(col,row) =
     *                                smooth_arr(col,row)+curr_element
  130                     continue
  140                 continue
                  end if
              end if
  150     continue
  160 continue

C     Now convert all the weights to reals and invert them.
      do 180, i = 1, n2
          do 170, j = 1, n1
              if ( smooth_arr( j, i ) .ne. 0 ) then
                  smooth_arr( j, i ) = 1.0 / smooth_arr( j, i )
              end if
  170     continue
  180 continue

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine GET_SMOOTH_ARRAY' )
          return
      end
