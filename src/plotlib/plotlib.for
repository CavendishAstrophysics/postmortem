C
C
C+plot_begin

      subroutine plot_begin( plot_device, s )
C     ---------------------------------------
C
C open the PGPLOT plot device
C
C Given:
C   default plot device
       character*(*)    plot_device
C returned
C   error return
       integer          s
C
C-
C local plot device
       character*40  local_device

       include '/mrao/post/include/plot_control.inc'

C check status on entry
       if (s.ne.0) return

C examine mode and take action
       if (plot_prompt.eq.1) then
          call io_getplt('Plot-device : ', plot_device, local_device, s)
          call pgbegin( 0, local_device, 1, 1 )
       else
          call pgbegin( 0, plot_device, 1, 1 )
       end if

C report any errors
       if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_BEGIN' )

       end
C
C+plot_complex
C
      SUBROUTINE plot_complex(    title,
     *                            header, footer,
     *                            comp_list,
     *                            num_pts, interval,
     *                            index,
     *                            plot_device,
     *                            plot_type,
     *                            s                      )

C
C     Plots complex visibilities on the plot device
C
C     Given:
C         Plot title, header and footer
              character*(*)       title(4), header, footer
C         Buffer of complex numbers
              complex             comp_list( * )
C         Number of complex numbers to plot
              integer             num_pts
C         Group interval between complex nos
              integer             interval
C         X axis values for each of these numbers
              real                index( num_pts )
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Type of plot - 1=cos,sine, 2=A,phi, 3 = both.
C         If it has any other value it will be prompted for.
              integer             plot_type
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Plots complex numbers vs a real number. The complex numbers
C     can be in a 2-d array if interval is other than 1. Numbers with
C     amplitude zero are not plotted.
C
C-
C     ==================================================================
C
C     Function declarations
C
      include    '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'

C
C     Local constant and variable declarations
C
C     Constants
C         The maximum number of points that can be plotted.
              integer         max_pts
              parameter     ( max_pts = 3000 )

C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i, j
C         List of reals to plot
              real            plot_list( max_pts )
              real            plot_index( max_pts )
C         Current PGPLOT character font and height
              integer         font
              real            height
C         View-Ports to use in multiple plot
              real            view_ports(4,2,9)
C         Buffer variables for plot-scaling parameters
              integer         save_plot_scale_type
              real*4          save_plot_limit_min
              real*4          save_plot_limit_max
C         Include Plot control information
              include '/mrao/post/include/plot_control.inc'

C
C Subroutine initialisation
C -------------------------
C
C Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( num_pts .ge. max_pts ) then
          s = ILL_BUFFSIZE
          goto 9999
      end if

C Suppress plotting of zero data values
      call plot_setzeros( 'NO', s )

C Initialise plot type to be Amp/phase or Cos/Sin
      if ( plot_type.lt.1 .or. plot_type.gt.3 ) then
          plot_type = plot_data_type
          if (plot_type.eq.0) then
            plot_type = plot_Ampphi
          end if
      end if

      if ( plot_type .eq. 0 ) s = usr_break
      if ( s .ne. 0 ) goto 9999

C save plot scaling parameters
      save_plot_scale_type = plot_scale_type
      save_plot_limit_min  = plot_limit_min
      save_plot_limit_max  = plot_limit_max
C define character attributes
      call pgqcf( font )
      call pgqch( height )
      call pgsch( 1.0  )
C define viewports
      if (plot_mode .ne. plot_multiple) then
        call plot_setvp(1,1,view_ports,s)
       else
        call plot_setvp(segment_x,segment_y,view_ports,s)
        call pgsch( sqrt(2.0)/sqrt(float(segment_x*segment_y)) )
       end if

C
C Main Code
C ---------
C
C Move to next logical page
      if (plot_page.eq.1 .or. plot_mode.ne.plot_multiple) then
        call pmadvance( s )
        plot_page = 1
      end if
      if (s .ne. 0) goto 9999

C buffer PGPLOT output
      call pgbbuf

      if ( plot_type.eq.1 .or. plot_type.eq.3 ) then
C .. Cosine, sine plot - first clear screen
C .. Plot cosine
          call pgvport( view_ports(1,1,plot_page),
     *                  view_ports(2,1,plot_page),
     *                  view_ports(3,1,plot_page),
     *                  view_ports(4,1,plot_page) )
          j = 1
          do 100, i = 1, num_pts
              plot_list(i) = real( comp_list( j ) )
              plot_index(i)= index(i)
              j = j + interval
  100     continue

          plot_upper = .true.
          plot_lower = .false.
          call plot_data( num_pts, plot_index, plot_list, 1.0, 1.0, s )
          call pgscf( 1 )
          if (plot_mode.eq.plot_normal) then
            call pglabel( ' ', 'Cosine (Jy)', header )
          else if (plot_mode.eq.plot_multiple) then
            if (plot_page.le.segment_y) then
              call pglabel( ' ', 'Cosine (Jy)', ' ' )
            end if
            call pgmtext( 'T', 0.3, 0.0, 0.0, title(2))
          else if (plot_mode.eq.plot_brief) then
            call pglabel( ' ', 'Cos (Jy)', ' ' )
          end if

C .. Plot sine
          call pgvport( view_ports(1,2,plot_page),
     *                  view_ports(2,2,plot_page),
     *                  view_ports(3,2,plot_page),
     *                  view_ports(4,2,plot_page) )
          j = 1
          do 200, i = 1, num_pts
              plot_list(i) = imag( comp_list( j ) )
              plot_index(i)= index(i)
              j = j + interval
  200     continue

          plot_upper = .false.
          plot_lower = .true.
          call plot_data( num_pts, plot_index, plot_list, 1.0, 1.0, s )
          if (plot_mode.eq.plot_normal) then
            call pglabel( footer, 'Sine (Jy)', ' ' )
          else if (plot_mode.eq.plot_multiple) then
            if ((plot_page-segment_y*(plot_page/segment_y)).eq.0) then
              call pglabel( footer , ' ', ' ' )
            end if
            if (plot_page.le.segment_y) then
              call pglabel( ' ', 'Sine (Jy)', ' ' )
            end if
           else if (plot_mode.eq.plot_brief) then
            call pglabel( footer, 'Sin (Jy)', ' ' )
          end if

      end if

      if ( plot_type.eq.2 .or. plot_type.eq.3 ) then
C .. Amplitude, phase plot - first clear screen
C .. Plot amplitude
          call pgvport( view_ports(1,1,plot_page),
     *                  view_ports(2,1,plot_page),
     *                  view_ports(3,1,plot_page),
     *                  view_ports(4,1,plot_page) )
          j = 1
          do 400, i = 1, num_pts
              plot_list(i) = abs( comp_list( j ) )
              plot_index(i)= index(i)
              j = j + interval
  400     continue

          plot_upper = .true.
          plot_lower = .false.
          call plot_data( num_pts, plot_index, plot_list, 1.0, 1.0, s )
          call pgscf( 1 )
          if (plot_mode.eq.plot_normal) then
            call pglabel( ' ', 'Amplitude (Jy)', header )
          else if (plot_mode.eq.plot_multiple) then
            if (plot_page.le.segment_y) then
              call pglabel( ' ', '(Jy)', ' ' )
            end if
            call pgmtext( 'T', 0.3, 0.0, 0.0, title(2))
          else if (plot_mode.eq.plot_brief) then
            call pglabel( ' ', '(Jy)', ' ' )
          end if

C .. Plot phase (range set to -180,180 degrees)
          plot_scale_type = 1
          plot_phase_type = 1
          if (plot_phi_max.gt.plot_phi_min) then
            plot_limit_min = plot_phi_min
            plot_limit_max = plot_phi_max
          else
            plot_limit_min  = -180.0
            plot_limit_max  =  180.0
          end if
          if (plot_mode.eq.plot_scaled_phi) then
            plot_scale_type = 0
            plot_phase_type = 0
          end if
          call pgvport( view_ports(1,2,plot_page),
     *                  view_ports(2,2,plot_page),
     *                  view_ports(3,2,plot_page),
     *                  view_ports(4,2,plot_page) )
          j = 1
          do 500, i = 1, num_pts
              if (comp_list(j) .ne. (0.0,0.0)) then
                  plot_list(i) =
     *                atan2( imag(comp_list(j)), real(comp_list(j)) ) /
     *                                                        const_d2r
              else
                  plot_list(i) = 0.0
              end if
              plot_index(i)= index(i)
              j = j + interval
  500     continue

          plot_upper = .false.
          plot_lower = .true.
          call plot_data( num_pts, plot_index, plot_list, 1.0, 1.0, s )
C .. restore plot scale type
          plot_upper = .false.
          plot_lower = .false.
          plot_phase_type = 0
          plot_scale_type = save_plot_scale_type
          plot_limit_min  = save_plot_limit_min
          plot_limit_max  = save_plot_limit_max
          if (plot_mode.eq.plot_normal) then
            call pglabel( footer, 'Phase (deg)', ' ' )
          else if (plot_mode.eq.plot_multiple) then
            if ((plot_page-segment_y*(plot_page/segment_y)).eq.0) then
              call pglabel( footer, ' ', ' ' )
            end if
            if (plot_page.le.segment_y) then
              call pglabel( ' ', '(deg)', ' ' )
            end if
          else if (plot_mode.eq.plot_brief) then
            call pglabel( footer, '(deg)', ' ' )
          end if

      end if

C     Plot title if this is the first plot on the page
      call pgvport( 0.1, 0.9, 0.85, 1.0 )
      call pgwindow( 0.0, 100.0, 4.2, -0.2 )
      if (plot_page.eq.1) then
        call pgscf( 0 )
        call pgsch( 1.0 )
        if (plot_mode.ne.plot_multiple) then
          do i = 1, 4
            call pgtext( 0.0, real(i), title(i) )
          end do
        else
          call pgtext( 0.0, 1.0, title(1) )
          call pgtext( 0.0, 2.0, title(3) )
          call pgtext( 0.0, 3.0, title(4) )
          call pgtext( 0.0, 4.0, header   )
        end if
      end if
      call pgebuf

C update page count
      plot_page = plot_page + 1
      if (plot_page.eq.full_page) then
        plot_page = 1
      end if
      call pgscf( font )
      call pgsch( height )

      return

C
C Error Handling
C --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call plot_wrerr( s, 'in subroutine PLOT_COMPLEX' )
          end if
          return
      end
C
C
C
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
          call vstats( num_plot, plot_y, stats, posns )
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
            call lsf_enq_ADC( ratel, ADC, s )
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
C
C
C+plot_enqmode
C
      subroutine plot_enqmode ( mode, s )
C
C Enquire the plot mode for use by plot_complex
C
C Returned:
C    Mode (normal or multiple)
       character*(*)      mode
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       mode = 'NORMAL'
       if (plot_mode.eq.plot_normal) then
         mode = 'NORMAL'
       elseif (plot_mode.eq.plot_multiple) then
         mode = 'MULTIPLE'
       elseif (plot_mode.eq.plot_brief) then
         mode = 'BRIEF'
       elseif (plot_mode.eq.plot_scaled_phi) then
         mode = 'SCALED-PHI'
       end if

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_ENQMODE' )

       end
C
C
C+ plot_getopts

      subroutine plot_getopts( plot_device, s )
C
C Set plot options interactively
C
C Returned:
C    default plot device
       character*(*)    plot_device
C    status word
       integer          s
C
C set plot options interactively.  The following options are defined:
C
C 1) AUTO-SCALING
C    turn auto-scaling of amplitude on
C
C 2) FIXED-SCALING
C    Parameters: lower_limit, upper_limit
C    define a fixed scaling for amplitude plots
C
C 3) PLOT-DEVICE
C    Parameters: device-name
C    define the plot device to use
C
C 4) PROMPT-STATE
C    Parameters: prompting-on/off
C    turn automatic prompting for output device on/off (default is off)
C
C 5) PHASE-LIMITS
C    Paramters: minimum-phase, maximum-phase
C    set the limits on the plots of phase
C
C 6) AMPLITUDE-PHASE-PLOTS
C    Parameters: none
C    plot amplitude and phase not cos/sin
C
C 7) COS-SIN-PLOTS
C    Parameters: none
C    plot cos and sin not amplitude/phase
C
C 8) BRIEF-DISPLAY
C    Parameters: on/off
C    turn brief display option on/off
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
C
C Local variables
C   options
       integer       number_options
       parameter    (number_options = 8)
       character*80  option_list(number_options), option

       data option_list(1)/
     * 'auto-scaling ......... turn auto-scaling of amplitude on'
     *                    /
       data option_list(2)/
     * 'fixed-scaling ........ define a fixed amplitude scale'
     *                    /
       data option_list(3)/
     * 'plot-device .......... specify a new default plot device'
     *                    /
       data option_list(4)/
     * 'prompt-state ......... use default-device (off) or prompt (on)'
     *                    /
       data option_list(5)/
     * 'phase-limits ......... limits on phase plots'
     *                    /
       data option_list(6)/
     * 'amplitude-phase-plot . plot amplitude and phase not cos/sin'
     *                    /
       data option_list(7)/
     * 'cos-sin-plots ........ plot cos and sin not amplitude/phase'
     *                    /
       data option_list(8)/
     * 'brief-display ........ turn brief display option on/off'
     *                    /

C check status on entry
       if (s.ne.0) return

C read option from the command line
       call io_getopt('Display-option (?=list) : ','auto-scaling',
     *             option_list,number_options,option,s)
       if (s.ne.0) goto 999


C decode options
       if (chr_cmatch(option,option_list(1))) then
         plot_scale_type = 0

       elseif (chr_cmatch(option,option_list(2))) then
         plot_scale_type = 1
         call io_getr('Amplitude lower-limit : ','0.0',plot_limit_min,s)
         call io_getr('Amplitude upper-limit : ','1.0',plot_limit_max,s)

       elseif (chr_cmatch(option,option_list(3))) then
          call io_getplt('Plot-device : ', '*', plot_device, s)

       elseif (chr_cmatch(option,option_list(4))) then
          if (io_onoff('Prompt-state : ','off',s)) then
            plot_prompt = 1
          else
            plot_prompt = 0
          end if

       elseif (chr_cmatch(option,option_list(5))) then
         call io_getr('Phase lower-limit : ','-180.0',plot_phi_min,s)
         call io_getr('Phase upper-limit : ','180.0',plot_phi_max,s)

       elseif (chr_cmatch(option,option_list(6))) then
         plot_data_type = plot_Ampphi

       elseif (chr_cmatch(option,option_list(7))) then
         plot_data_type = plot_cossin

       elseif (chr_cmatch(option,option_list(8))) then
         if (io_onoff('Brief output option : ','off',s)) then
           call plot_setmode('BRIEF',s)
         else
           call plot_setmode('NORMAL',s)
         end if

       end if

999   continue
      if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_GETOPTS' )
      end


*+plot_metafile

       subroutine plot_metafile( s )
C
C Plot a metafile on an output device
C
C Returned:
C   status
       integer       s
C
C-
       character*64  file,device
c
       if (s.ne.0) return

       file='PGPLOT:GMF'
       call io_enqplt(0,device)
       call io_getfil('metafile name : ','*',file,s)
       call io_getplt('graphics device/type : ','*',device,s)
       if (s.eq.0) then
          call gmfplot(file,device)
       endif
       if (s.ne.0) call io_wrerr( s, 'in plot_metafile' )
       end
C
C
C+plot_set_fit
C
      subroutine plot_set_fit ( option, s )
C
C Determines the optional plotting of fits to Phi plots
C
C Input:
C    option to turn on plots of fit (YES/NO)
       character*(*)      option
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (s.ne.0) return

       plot_fit = chr_cmatch(option(1:chr_lenb(option)),'YES')

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SET_FIT' )

       end
C
C
C+plot_setmode
C
      subroutine plot_setmode ( mode, s )
C
C Sets the plot mode for use by plot_complex
C
C Input:
C    Mode (normal or multiple)
       character*(*)      mode
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C local function
       logical   chr_cmatch

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       if (chr_cmatch(mode,'NORMAL')) then
         plot_mode = plot_normal
       else if (chr_cmatch(mode,'MULTIPLE')) then
         plot_mode = plot_multiple
       else if (chr_cmatch(mode,'BRIEF')) then
         plot_mode = plot_brief
       else if (chr_cmatch(mode,'SCALED-PHI')) then
         plot_mode = plot_scaled_phi
       else
         plot_mode = ill_pltmode
       end if
C reset page number to the start of the plot
       plot_page = 1

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETMODE' )

       end
C
C
C+plot_setpage

      subroutine plot_setpage( items, s )
C
C Set the number of items needed on a single plot in multiple plot mode
C
C Input:
C    Number of plots needed
       integer           items
C Returned:
C    Status
       integer           s
C-

       include   '/mrao/post/include/plot_control.inc'
       include   '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C check for a call to plot_setmode
       if (plot_mode.ne.plot_multiple) then
         call io_wrout( '*** No call to PLOT_SETMODE ( multiple ) ' )
         s = ill_pltmode
         goto 999
       end if

C check number of items does not exceed maximum
       if (items.gt.9) then
         s = ill_pltmlt
       else
         full_page = items
         plot_page = 1
         if (items.eq.1) then
           segment_x = 1
           segment_y = 1
         else if (items.eq.2) then
           segment_x = 1
           segment_y = 2
         else if (items.le.4) then
           segment_x = 2
           segment_y = 2
         else if (items.le.6) then
           segment_x = 2
           segment_y = 3
         else
           segment_x = 3
           segment_y = 3
         end if
       end if

999    if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETPAGE')

       end
C
C
C+plot_setscale
C
      subroutine plot_setscale ( auto_scaling, min_y, max_y, s )
C
C Sets the type and range for the plot y scale
C
C Input:
C    type of scaling
       logical            auto_scaling
C    range for y-scale
       real*4             min_y, max_y
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       if (auto_scaling) then
         plot_scale_type = 0
       else
         plot_scale_type = 1
         plot_limit_min  = min_y
         plot_limit_max  = max_y
       end if
       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETSCALE' )

       end
C
C
C+plot_setvp

       subroutine plot_setvp( nx, ny, vp, s )
C      --------------------------------------
C
C Find suitable values for viewports in a multiple plot
C
C Input:
C    Number of segments in x and y
       integer       nx, ny
C Returned:
C    View-ports
       real*4        vp(4,2,*)
C    Status
       integer       s
C-

C Local variables
       integer     ntot, n1, n2
       real*4      size_x, size_y

C check status on entry
       if ( s.ne.0 ) return

C move through the range of windows
       ntot = 0
       size_x = .9 / float(nx)
       size_y = .75 / float(ny)
       do n1 = 1, nx
         do n2 = 1, ny
           ntot = ntot + 1
           if (n1.eq.1) then
             vp(1,1,ntot) = 0.05 + (n1-1)*size_x + size_x*0.075
             vp(1,2,ntot) = vp(1,1,ntot)
           else
             vp(1,1,ntot) = 0.05 + (n1-1)*size_x
             vp(1,2,ntot) = vp(1,1,ntot)
           end if
           vp(2,1,ntot) = 0.05 + n1*size_x - size_x*0.075
           vp(2,2,ntot) = vp(2,1,ntot)
           vp(4,1,ntot) = 0.80 - (n2-1)*size_y - 0.075*size_y
           vp(3,1,ntot) = vp(4,1,ntot) - size_y/2.0 + 0.125*size_y
           vp(3,2,ntot) = 0.80 - n2*size_y + 0.075*size_y
           vp(4,2,ntot) = vp(3,2,ntot) + size_y/2.0 - 0.125*size_y
         end do
       end do

       if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_SETVP')
       end

C+plot_setzeros
C
      subroutine plot_setzeros ( option, s )
C
C Determines the optional plotting of zero data values
C
C Input:
C    option to turn on plots of zero data values (YES/NO)
       character*(*)      option
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

       if (s.ne.0) return

       plot_zeros = chr_cmatch(option(1:chr_lenb(option)),'YES')

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETZEROS' )

       end


*+plot_wrerr

       subroutine plot_wrerr (status, text)
C
C  Writes an error message to the error device.
C
C  Given:
C      STATUS    integer     status value
C      TEXT      char*(*)    message text
C
C  Writes an error message to the error device, appropriate to the given
C  status value, and including the message text if this is not blank.
C  The message is also written to the error log file if this has been
C  set up by a previous call to IOLIB routine io_setlog.
C
C  This routine sets up the error message file for the SAMPLIB library
C  and calls the IOLIB routine io_wrerr.
C
*-
       character  text*(*)
       integer    istat, status
C
       character   errfil*34
       parameter ( errfil = '/mrao/post/include/plot-errors.inc')
C
include '/mrao/include/iolib_errors.inc'
C
       istat=IOLIB_OK
       call io_setmsg(errfil,istat)
       if (istat.ne.IOLIB_OK) call io_wrerr(istat,errfil)
C
       call io_wrerr(status,text)
C
       end





C
C
C+pmadvance
C
      subroutine pmadvance ( s )

C     Duplicates PGADVANCE for postmortem, returning a status.
C
C     Given:
C         None.

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine is essentially the same as PGADVANCE, but if the
C     user types <esc>, Q or E instead of <RETURN> then a status of
C     USR_BREAK is returned. Also no prompting is done and the PGPLOT
C     ask flag is set to the default for the current plot device - on
C     if interactive, otherwise off.
C
C     NPR     13 October 1987.
C-
C     ==================================================================
C
C     Function declarations

      include        '/mrao/include/chrlib_functions.inc'

C
C     Global constant declarations

      include        '/mrao/include/iolib_errors.inc'

C
C     Local variable declarations
C         The "advance" state of PGPLOT.
              integer             adv_state
C         The "prompt" state of PGPLOT.
              logical             prompt
C         Users keyboard response
              character*1         response

C
C Main Code
C ---------
C
      if ( s .ne. 0 ) return

      call pgqask( prompt )
      call pgqadv( adv_state )

      if ((adv_state.eq.1) .and. prompt ) then
          call io_setech( .false. )
          call io_getkey( ' ',' ','Qq',response,s)
          call io_setech( .true. )
          if ( chr_cmatch( response, 'Q' ) ) s = usr_break
      end if

      call pgask( .false. )
      if ( s .eq. 0 ) then
          call pgadvance
      end if

      call pgask( prompt )

      return

C
C Error Handling
C --------------
C
 9999 continue
          call plot_wrerr( s, 'in subroutine PMADVANCE' )
          return
      end
 
C+VSTATS

      SUBROUTINE VSTATS( N, A, STATS, POSNS )
C
C     Returns statistics on a vector.
C
C     Given
C         Number of vector elements
              INTEGER     N
C         Vector
              REAL        A(N)

C     Returned
C         Minimum, maximum, mean and standard deviation of the vector.
              REAL        STATS(4)
C         Position of the minimum and maximum in the vector
              INTEGER     POSNS(2)

C     If the array has only one element, the standard deviation is
C     zero, and all the other statistics refer to the only element.
C     If N is less than one the routine is a null routine.
C
C     NPR     4 Feb 1988
C
C-
      REAL*8      SUM, SUM_SQR
      INTEGER     I

      IF (N .GE. 1) THEN
          STATS(1) = A(1)
          STATS(2) = A(1)
          POSNS(1) = 1
          POSNS(2) = 1
          SUM      = A(1)
          SUM_SQR  = A(1)*A(1)

          DO 100, I = 2, N
              SUM     = SUM     + A(I)
              SUM_SQR = SUM_SQR + A(I)*A(I)
              IF (A(I) .LT. STATS(1)) THEN
                  STATS(1) = A(I)
                  POSNS(1) = I
              END IF
              IF (A(I) .GT. STATS(2)) THEN
                  STATS(2) = A(I)
                  POSNS(2) = I
              END IF
  100     CONTINUE

          STATS(3) = SUM/REAL(N)
          STATS(4) = SUM_SQR/REAL(N)-STATS(3)*STATS(3)
          STATS(4) = SQRT(MAX1(0.0, STATS(4)))
      END IF

      RETURN
      END
