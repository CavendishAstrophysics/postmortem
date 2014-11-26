C
C+map_plot
C
      SUBROUTINE map_plot(   plot_device, s )

C
C     Plots the mapper functions on the plot device.
C
C     Given:
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Takes the current convolution function defined in the map redtape
C     and plots it on the plot device.
C
C     NPR     30 September 1987.
C
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
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/maplib_redtape.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         The maximum half width of the convolution function.
              integer         max_pts
              parameter     ( max_pts = 400 )
C         The maximum width of the map.
              integer         max_map_width
              parameter     ( max_map_width = 1024 )

C     Variables, equivilances and commons
C         Loop counter
              integer         i
C         The PGPLOT index array.
              real            index( 2*max_pts+1 )
C         The convolution function
              real            conv( -max_pts:max_pts )
C         The correction or grading function
              real            corr( max_map_width+2 )
C         The aperture scale parameters
              real            u_wl2gp, v_wl2gp
              real*8          aper_skew
C         The actual half width of the convolution function
              integer         conv_pts
C         Flags set to indicate which plots to do
              logical         plot_conv, plot_corr, plot_grad
              character*3     default

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      default = 'Yes'
      plot_conv = io_yesno( 'Plot convolution function ? ', default, s )
      if (plot_conv) default = 'No'
      plot_corr = io_yesno( 'Plot correction function  ? ', default, s )
      if (plot_conv.or.plot_corr) default = 'No'
      plot_grad = io_yesno( 'Plot grading function  ? ', default, s )
      if (.not. (plot_corr.or.plot_conv.or.plot_grad)) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      conv_pts = convos*convhw
      call tabfn( convtp, convos, convhw, convpa, max_pts, conv, s )
      if ( s .ne. 0 ) goto 9999

      call pgbegin( 0, plot_device, 1, 1 )
      if (plot_conv) then
          do 100, i = 0, 2*conv_pts
              index(i+1) = real(i-conv_pts)/real(convos)
  100     continue

          call pgbbuf
          call pgenv(real(-convhw), real(convhw), -0.25, 1.0, 0,0 )
          call pglabel( 'Gridpoints from u,v position.', ' ',
     *                     'Convolution function' )
          call pgline( 2*conv_pts+1, index, conv(-conv_pts) )
          call pgebuf
      end if

      if (plot_corr) then
          call corrfn( convhw, convos, max_pts, conv,
     *                 corrtp, ixmax, corr, s          )
          if ( s .ne. 0 ) goto 9999

          do 200, i = 1, ixmax
              index(i) = real(i+iumap1-1)
              corr(i)  = log10(abs(corr(i)))
  200     continue

          if (plot_conv) call pmadvance( s )
          if (s.ne.0) goto 9999

          call pgbbuf
          call pgwindow( real(iumap1), real(iumap2), -1.0, 3.0 )
          call pgbox( 'BCNST', 0.0, 0, 'BCLNST', 0.0, 0 )
          call pglabel( 'Map U gridpoint.',' ',
     *                  'Log. map correction amplitude.' )
          call pgline( ixmax, index, corr )
          call pgebuf
      end if

      if (plot_grad) then
C         Fudge it into thinking it is a 1-D aperture
          do 300, i = 1, ixmax+2, 2
              corr(i)   = 1.0
              corr(i+1) = 0
  300     continue

          call enaper( u_wl2gp, v_wl2gp, aper_skew, s )
          call grade_aperture(  corr,
     *                          int(ixmax/2+1), 1,
     *                          u_wl2gp, v_wl2gp,
     *                          gradtp, gradpa,
     *                          0, 0, s               )

C         Resort array.
          do 400, i = 1, ixmax/2+1
              corr(i) = corr(2*i-1)
  400     continue
          do 500, i = 2, ixmax/2
              corr(ixmax-i+2) = corr(i)
  500     continue
          do 600, i = 1, ixmax
              index(i) = real(i+iumap1-1)
  600     continue

          if (plot_conv.or.plot_corr) call pmadvance( s )
          if (s.ne.0) goto 9999

          call pgbbuf
          call pgenv( real(iumap1), real(iumap2), 0.0, 1.0, 0,0 )
          call pglabel( 'Map U gridpoint.',' ','Map grading function.' )
          call pgline( ixmax, index, corr )
          call pgebuf
      end if

      call pgend

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call map_wrerr( s, 'in subroutine MAP_PLOT_CONV' )
          end if
          return
      end
