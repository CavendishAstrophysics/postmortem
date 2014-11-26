C+plot_data
C
      SUBROUTINE plot_data(   num_plot,
     *                        plot_x,
     *                        plot_y,
     *                        alt_scale_x,
     *                        alt_scale_y,
     *                        s                      )

C
C     Plots data within a predefined PGPLOT window.
C
C     Given:
C         Number of points to plot
              integer         num_plot
C         List of reals to plot - x first, then y.
              real            plot_x( num_plot )
              real            plot_y( num_plot )
C         Alternative scales for x and y (see below)
              real            alt_scale_x, alt_scale_y
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Plots given data in a predefined pgplot window. The plot has a
C     full frame with an x axis plotted and the minimum, maximum, mean
C     and standard deviation of the y values are plotted within the
C     frame. The two parameters alt_scale_x and alt_scale_y allow the
C     plotting of two x scales or two y scales. If alt_scale_x does not
C     equal 1 then the scale on the top border of the graph is
C     alt_scale_x times the scale on the bottom border. Similarly
C     alt_scale_y defines an alternative scale for the right border.
C
C-
C     ==================================================================
C
C     Function declarations
C
      include    '/mrao/include/iolib_functions.inc'
      include    '/mrao/include/chrlib_functions.inc'
      real        pgrnd

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'

C
C     Local constant and variable declarations
C         Statistics on plot and plotting of fits for PHI data
              real            stats(4), ratel, ADC(3), ha
              integer         posns(2)
C         Maximum and minimum plot values
              real            max_x, min_x, max_y, min_y
C         Y axis major tick interval and number of subticks/tick
              real            tick_intvl, stick_intvl
              integer         ntick
C         Character representations of numbers
              character*10    num1, num2, num3, num4
C         Output line and string length
              character*20    line1, line2, line3, line4
              integer         ls
C         Start plot and plot length indicators
              integer         sp, lp
              integer         i
C
C         Make control information available
              include '/mrao/post/include/plot_control.inc'
C         Constants
              include '/mrao/include/constants.inc'

C
C Subroutine initialisation
C -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C Main Code
C ---------
C

      if (num_plot .eq. 1) then
C         Convert to a two point plot.
          plot_y(2) = plot_y(1)
          plot_x(2) = plot_x(1) + 1.0
          plot_x(1) = plot_x(1) - 1.0
          num_plot  = 2
      end if

      if (num_plot .le. 0) then
          call pgwindow( -1.0, 1.0, -1.0, 1.0 )
          call pgbox( 'BC', 0.0, 0, 'BC', 0.0, 0 )
          call pgtext( -0.25, 0.0, 'No data for this plot' )
      else
C         Find plot statistics and reasonable plot minimum and maximum
          call plot_vstats( num_plot, plot_y, stats, posns )
          if (plot_scale_type.eq.1) then
            min_y = plot_limit_min
            max_y = plot_limit_max
          else
            min_y = min(stats(1), 0.0)
            max_y = max(stats(2), 0.0)
          end if
          if (min_y .eq. max_y) max_y = 1.0

C         Set the tick interval
          if (plot_phase_type.eq.1) then
            tick_intvl = 90.0
            ntick = 1
          else
            tick_intvl = pgrnd( (max(max_y,abs(min_y))/2.5), ntick )
            stick_intvl = abs( tick_intvl/real(ntick) )
            if (amod(-min_y,stick_intvl) .ne. 0.0) then
                min_y = -aint(1.0-min_y/stick_intvl)*stick_intvl
            end if
            if (amod(max_y,stick_intvl) .ne. 0.0) then
                max_y =  aint(1.0+max_y/stick_intvl)*stick_intvl
            end if
          end if

          if (plot_x(1).eq.plot_x(num_plot)) then
              min_x = plot_x(1) - 1.0
              max_x = plot_x(1) + 1.0
          else
              min_x = plot_x(1)
              max_x = plot_x(num_plot)
              do i = 1, num_plot
                 min_x = min(min_x,plot_x(i))
                 max_x = max(max_x,plot_x(i))
              enddo
          end if

C         Plot the data.
          call pgwindow( min_x, max_x, min_y, max_y )
          if (plot_zeros) then
              call pgline( num_plot, plot_x, plot_y )

          else
              sp = 1
              do while (sp .lt. num_plot)
                  do while (sp.le.num_plot .and. plot_y(sp) .eq. 0.0)
                      sp = sp + 1
                  end do

                  if (sp .le. num_plot) then
                      lp = 1
                      do while ( (sp+lp).le.num_plot .and.
     *                           plot_y(sp+lp).ne.0.0       )
                          lp = lp + 1
                      end do
                      if (lp .eq. 1) then
                          call pgpoint( 1, plot_x(sp), plot_y(sp), 2 )
                      else if (lp .gt. 1) then
                          call pgline( lp, plot_x(sp), plot_y(sp) )
                      end if
                      sp = sp + lp
                  end if
              end do
          end if

C         Plot fit if required
          if (plot_fit .and. plot_lower) then
c           call lsf_enq_ADC( ratel, ADC, s )
            do sp=1,num_plot
              ha = plot_x(sp)*const_2pi/24.0 - ratel
              plot_y(sp) = ADC(1)*cos(ha)+ADC(2)*sin(ha)+ADC(3)
              if (plot_y(sp).gt.180.0) then
                do while (plot_y(sp).gt.180.0)
                  plot_y(sp) = plot_y(sp) - 360.0
                end do
              else if (plot_y(sp).lt.-180.0) then
                do while (plot_y(sp).lt.-180.0)
                  plot_y(sp) = plot_y(sp) + 360.0
                end do
              end if
            end do
            call pgline(num_plot,plot_x,plot_y)
            call chr_chrtoc( ADC(1), num1, ls )
            call chr_chrtoc( ADC(2), num2, ls )
            call chr_chrtoc( ADC(3), num3, ls )
            line1 = 'A = ' // num1
            line2 = 'D = ' // num2
            line3 = 'C = ' // num3
            line4 = ' '
          else

            call chr_chrtoc( stats(1), num1, ls )
            call chr_chrtoc( stats(2), num2, ls )
            call chr_chrtoc( stats(3), num3, ls )
            call chr_chrtoc( stats(4), num4, ls )
            line1 = 'Min. ' // num1
            line2 = 'Max. ' // num2
            line3 = 'Mean ' // num3
            line4 = 'S.D. ' // num4
          end if

C         add additional title to this plot
C         Print the plot statistics
          call pgwindow( 0.0, 100.0, 10.8, -0.2 )

          if (plot_mode.eq.plot_normal) then
            if (stats(3) .le. ((max_y+min_y)/2.0)) then
              call pgtext( 50.0,  1.0, line1 )
              call pgtext( 80.0,  1.0, line2 )
              call pgtext( 50.0,  2.0, line3 )
              call pgtext( 80.0,  2.0, line4 )
            else
              call pgtext( 50.0,  9.0, line1 )
              call pgtext( 80.0,  9.0, line2 )
              call pgtext( 50.0, 10.0, line3 )
              call pgtext( 80.0, 10.0, line4 )
            end if
          end if

C         Finally, draw the box frame.
          call pgwindow( min_x, max_x, min_y, max_y )
          if (alt_scale_x .eq. 1.0 .and. alt_scale_y .eq. 1.0) then
            if ((plot_mode.eq.plot_brief.or.plot_mode.eq.plot_multiple)
     *           .and. plot_upper) then
              call pgbox( 'ABCT', 0.0, 0, 'BCNST', tick_intvl, ntick )
            else
              call pgbox( 'ABCNST', 0.0, 0, 'BCNST', tick_intvl, ntick )
            end if
          else
              if (max_y .ne. 0) then
                  call pgbox('ABNST',0.0,0, 'BNST',tick_intvl,ntick )
              else
                  call pgbox('BNST',0.0,0, 'BNST',tick_intvl,ntick )
              end if

              min_x = min_x * alt_scale_x
              max_x = max_x * alt_scale_x
              min_y = min_y * alt_scale_y
              max_y = max_y * alt_scale_y
              tick_intvl = pgrnd( (max(max_y,abs(min_y))/2.5), ntick )
              call pgwindow( min_x, max_x, min_y, max_y )
              call pgbox('CMST',0.0,0, 'CMST',tick_intvl,ntick )
          end if
      end if

      if ( s .eq. 0 ) return

C
C Error Handling
C --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call plot_wrerr( s, 'in subroutine PLOT_DATA' )
          end if
          return
      end
