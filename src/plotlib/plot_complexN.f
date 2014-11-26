
C+plot_complexN

      SUBROUTINE plot_complexN    (title,
     *                            header, footer,
     *                            comp_list,
     *                            num_pts, interval,
     *                            index,
     *                            plot_device,
     *                            plot_type,
     *                            lsf_num, s)

C
C Plots complex visibilities on the plot device, with optional phase fit
C
C     Given:
C         Plot title, header and footer
              character*(*)       title(4), header, footer
C         Buffer of complex numbers
              complex             comp_list (*)
C         Number of complex numbers to plot
              integer             num_pts
C         Group interval between complex nos
              integer             interval
C         X axis values for each of these numbers
              real                index (num_pts)
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Type of plot - 1=cos,sine, 2=A,phi, 3 = both.
C         If it has any other value it will be prompted for.
              integer             plot_type
C lsf number
                integer         lsf_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Plots complex numbers vs a real number. The complex numbers
C     can be in a 2-d array if interval is other than 1. Numbers with
C     amplitude zero are not plotted.
C     If plot_fit is true, attempts to fit A*cos(H)+D*sin(H)+C to phase
C last mod 10 July 2003/13 Aug 2003  GP
C-
C     ==================================================================
C
C     Function declarations
C
      include    '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C          (i.e. Global constant, variable and common declarations)
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
              parameter      (max_pts = 6000)

C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i, j
              integer           sf_lun   ! used in phase-fit
              real*8            ratel    ! used in phase-fit
C         List of reals to plot
              real            plot_list  (max_pts)
              real            plot_index (max_pts)
C         Current PGPLOT character font and height
              integer         font
              real            height
C         View-Ports to use in multiple plot
              real            view_ports (4,2,9)
C         Buffer variables for plot-scaling parameters
              integer         save_plot_scale_type
              real*4          save_plot_limit_min
              real*4          save_plot_limit_max
C         Include Plot control information
              include '/mrao/post/include/plot_control.inc'

                integer out     !  from io_enqout: output unit
* for sla_dmat
                integer         iwork(3)
                real*8          c(3)
                real*8          matrix(3, 3)
* for fitting
                real*8          sumPc, sumPs, sumP
                real*8          sumc,  sums,  sum
                real*8          sumcc, sumcs, sumss

                real            last_phase, ch, sh, det
                real            ha, phase

       real*4                 save_ratel, save_ADC(3)
       common /save_fit_res/  save_ratel, save_ADC




C Subroutine initialisation
C -------------------------

C entry status
      if  (s .ne. 0)  return

      if  (num_pts .ge. max_pts)  then
          s = ILL_BUFFSIZE
          goto 9999
      end if


        call io_enqout(out)

C Suppress plotting of zero data values
      call plot_setzeros ('NO', s)

C Initialise plot type to be Amp/phase or Cos/Sin
      if  (plot_type.lt.1 .or. plot_type.gt.3)  then
          plot_type = plot_data_type
          if (plot_type.eq.0) then
            plot_type = plot_Ampphi
          end if
      end if

      if  (plot_type .eq. 0)  s = usr_break
      if  (s .ne. 0)  goto 9999

C save plot scaling parameters
      save_plot_scale_type = plot_scale_type
      save_plot_limit_min  = plot_limit_min
      save_plot_limit_max  = plot_limit_max
C define character attributes
      call pgqcf (font)
      call pgqch (height)
      call pgsch (1.0 )
C define viewports
      if (plot_mode .ne. plot_multiple) then
        call plot_setvp(1,1,view_ports,s)
       else
        call plot_setvp(segment_x,segment_y,view_ports,s)
        call pgsch (sqrt(2.0)/sqrt(float(segment_x*segment_y)))
       end if

C
C Main Code
C ---------
C
C Move to next logical page
      if (plot_page.eq.1 .or. plot_mode.ne.plot_multiple) then
        call pmadvance (s)
        plot_page = 1
      end if
      if (s .ne. 0) goto 9999

C buffer PGPLOT output
      call pgbbuf

      if  (plot_type.eq.1 .or. plot_type.eq.3)  then
C .. Cosine, sine plot - first clear screen
C .. Plot cosine
          call pgvport (view_ports(1,1,plot_page),
     *                  view_ports(2,1,plot_page),
     *                  view_ports(3,1,plot_page),
     *                  view_ports(4,1,plot_page))
          j = 1
          do 100, i = 1, num_pts
              plot_list(i) = real (comp_list (j) )
              plot_index(i)= index(i)
              j = j + interval
  100     continue

          plot_upper = .true.
          plot_lower = .false.
          call plot_data (num_pts, plot_index, plot_list, 1.0, 1.0, s)
          call pgscf (1)
          if (plot_mode.eq.plot_normal) then
            call pglabel (' ', 'Cosine (Jy)', header)
          else if (plot_mode.eq.plot_multiple) then
            if (plot_page.le.segment_y) then
              call pglabel (' ', 'Cosine (Jy)', ' ')
            end if
            call pgmtext ('T', 0.3, 0.0, 0.0, title(2))
          else if (plot_mode.eq.plot_brief) then
            call pglabel (' ', 'Cos (Jy)', ' ')
          end if

C .. Plot sine
          call pgvport (view_ports(1,2,plot_page),
     *                  view_ports(2,2,plot_page),
     *                  view_ports(3,2,plot_page),
     *                  view_ports(4,2,plot_page))
          j = 1
          do 200, i = 1, num_pts
              plot_list(i) = imag (comp_list (j) )
              plot_index(i)= index(i)
              j = j + interval
  200     continue

          plot_upper = .false.
          plot_lower = .true.
          call plot_data (num_pts, plot_index, plot_list, 1.0, 1.0, s)
          if (plot_mode.eq.plot_normal) then
            call pglabel (footer, 'Sine (Jy)', ' ')
          else if (plot_mode.eq.plot_multiple) then
            if ((plot_page-segment_y*(plot_page/segment_y)).eq.0) then
              call pglabel (footer , ' ', ' ')
            end if
            if (plot_page.le.segment_y) then
              call pglabel (' ', 'Sine (Jy)', ' ')
            end if
           else if (plot_mode.eq.plot_brief) then
            call pglabel (footer, 'Sin (Jy)', ' ')
          end if

      end if

      if  (plot_type.eq.2 .or. plot_type.eq.3)  then
C .. Amplitude, phase plot - first clear screen
C .. Plot amplitude
          call pgvport (view_ports(1,1,plot_page),
     *                  view_ports(2,1,plot_page),
     *                  view_ports(3,1,plot_page),
     *                  view_ports(4,1,plot_page))
          j = 1
          do 400, i = 1, num_pts
              plot_list(i) = abs (comp_list (j) )
              plot_index(i)= index(i)
              j = j + interval
  400     continue

          plot_upper = .true.
          plot_lower = .false.
          call plot_data (num_pts, plot_index, plot_list, 1.0, 1.0, s)
          call pgscf (1)
          if (plot_mode.eq.plot_normal) then
            call pglabel (' ', 'Amplitude (Jy)', header)
          else if (plot_mode.eq.plot_multiple) then
            if (plot_page.le.segment_y) then
              call pglabel (' ', '(Jy)', ' ')
            end if
            call pgmtext ('T', 0.3, 0.0, 0.0, title(2))
          else if (plot_mode.eq.plot_brief) then
            call pglabel (' ', '(Jy)', ' ')
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
          call pgvport (view_ports(1,2,plot_page),
     *                  view_ports(2,2,plot_page),
     *                  view_ports(3,2,plot_page),
     *                  view_ports(4,2,plot_page))
          j = 1
          do 500, i = 1, num_pts
              if (comp_list(j) .ne. (0.0,0.0)) then
                  plot_list(i) =
     *                atan2 (imag(comp_list(j)), real(comp_list(j)))  /
     *                                                        const_d2r
              else
                  plot_list(i) = 0.0
              end if
              plot_index(i)= index(i)
              j = j + interval
  500     continue

          plot_upper = .false.
          plot_lower = .true.
*          call plot_data (num_pts, plot_index, plot_list, 1.0, 1.0, s)

* fit to phase?

        if (plot_fit) then

                call lsf_enq_sf (lsf_num, sf_lun, i, s)
                call enq_ratel  (sf_lun, ratel, s)
                save_ratel = ratel

                sumP  = 0.0D0
                sumPc = 0.0D0
                sumPs = 0.0D0

                sumc  = 0.0D0
                sums  = 0.0D0
                sum   = 0.0D0

                sumcc = 0.0D0
                sumcs = 0.0D0
                sumss = 0.0D0


* remove phase jumps, get HA from ST

                do i = 1, num_pts
                    HA = index(i)*const_2pi/24.0 - RAtel
                    if (i .eq. 1) then
                        last_phase = plot_list(1)
                        phase      = plot_list(1)
** (A=0.0?)
                    else
                        phase = plot_list(i)
                        do while (phase-last_phase .lt.-180.0)
                            phase = phase + 360.0
                        enddo
                        do while (phase-last_phase .ge.+180.0)
                            phase = phase - 360.0
                        enddo
                        last_phase = phase
                    endif


                        cH = cos(HA)
                        sH = sin(HA)

                        sumP  =  sumP  + phase
                        sumPc =  sumPc + phase*cH
                        sumPs =  sumPs + phase*sH

                        sumc  =  sumc  + cH
                        sums  =  sums  + sH
                        sum   =  sum   + 1.0

                        sumcc =  sumcc + cH*cH
                        sumcs =  sumcs + cH*sH
                        sumss =  sumss + sH*sH
                enddo


                c(1) = sumPc
                c(2) = sumPs
                c(3) = sumP

                matrix(1,1) = sumcc
                matrix(2,1) = sumcs
                matrix(3,1) = sumc

                matrix(1,2) = sumcs
                matrix(2,2) = sumss
                matrix(2,3) = sums

                matrix(3,1) = sumc
                matrix(3,2) = sums
                matrix(3,3) = sum
*



                call sla_dmat (3, matrix, c, det, s, iwork)
                if (s .ne. 0) goto 9999

                do i = 1, 3
                    save_ADC(i) = C(i)
                enddo

        endif

           call plot_data (num_pts, plot_index, plot_list, 1.0, 1.0, s)


C .. restore plot scale type
          plot_upper = .false.
          plot_lower = .false.
          plot_phase_type = 0
          plot_scale_type = save_plot_scale_type
          plot_limit_min  = save_plot_limit_min
          plot_limit_max  = save_plot_limit_max
          if (plot_mode.eq.plot_normal) then
            call pglabel (footer, 'Phase (deg)', ' ')
          else if (plot_mode.eq.plot_multiple) then
            if ((plot_page-segment_y*(plot_page/segment_y)).eq.0) then
              call pglabel (footer, ' ', ' ')
            end if
            if (plot_page.le.segment_y) then
              call pglabel (' ', '(deg)', ' ')
            end if
          else if (plot_mode.eq.plot_brief) then
            call pglabel (footer, '(deg)', ' ')
          end if

      end if

C     Plot title if this is the first plot on the page
      call pgvport (0.1, 0.9, 0.85, 1.0)
      call pgwindow (0.0, 100.0, 4.2, -0.2)
      if (plot_page.eq.1) then
        call pgscf (1)
        call pgsch (1.0)
        if (plot_mode.ne.plot_multiple) then
          do i = 1, 4
            call pgtext (0.0, real(i), title(i))
          end do
        else
          call pgtext (0.0, 1.0, title(1))
          call pgtext (0.0, 2.0, title(3))
          call pgtext (0.0, 3.0, title(4))
          call pgtext (0.0, 4.0, header  )
        end if
      end if
      call pgebuf


        if (plot_fit) then

*          write (out, *)    ' summations: p, p*c, p*s'
*          write (out, 10000)  sumP, sumPc, sumPs

*          write (out, * )   ' matrix'
*          write (out, 10000)
*     *              sumcc, sumcs, sumc,
*     *              sumcs, sumss, sums,
*     *              sumc,  sums,  sum

*          write (out, *)    ' A, D, C'
*          write (out, 10000) save_ADC

        endif

10000   format (1x,  3(x, F10.1))

C update page count
      plot_page = plot_page + 1
      if (plot_page.eq.full_page) then
        plot_page = 1
      end if
      call pgscf (font)
      call pgsch (height)

      return

C
C Error Handling
C --------------
C
 9999 continue
          if  (s .ne. usr_break)  then
              call plot_wrerr (s, 'in subroutine PLOT_COMPLEX')
          end if
          return
      end
