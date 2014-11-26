C
C+ion_disp_samp
C
      SUBROUTINE ion_disp_samp( sf_name,
     *                          plot_device,
     *                          s               )

C
C     Displays the current ionospheric correction, sample by sample.
C
C     Given:
C         Physical sample file name - file should be closed.
              character*(*)       sf_name
C         Plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
*     include  '/mrao/include/pgplot_common.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/ionsys_errors.inc'
      include  '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variable declarations
C         General purpose loop counters
              integer         i
C         1-D Aperture and map array - complex before and real after
              complex         aperture( max_aper_size )
              real            map( max_map_size )
              equivalence   ( aperture, map )
C         Plot title
              character*(80)  title(4)
C         Plot index.
              real            index( max_map_size )
C         Plot min and max, and start, finish and length pointers
              real            start, stop, min_plot, max_plot
              integer         plot_start, plot_stop, plot_size
C         Display radius in arc seconds.
              real            disp_rad
C         LSF open number.
              integer         lsf_num
C         Buffer range to display, the current buffer, and sampling rate
              integer         first_buff, last_buff, buff_num, samp_rate
C         The search centre as a map gridpoint
              integer         search_posn
C         The conversion factor from radian/wavelength to degrees/km.
              real            rpwl2dpkm
C         The approximate conversion from sec/arc to degrees/km.
              real            sa2dpkm
C         Sample file unit number, source, and observing frequency.
              integer         sf_lun, src_num
              real*8          freq
C         Ionospheric correction buffer
              real            ion(4)

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      if ( ion_type .eq. 0 ) then
        call io_wrout('You cannot display old corrections in this way.')
        return
      end if

      call lsf_open( sf_name, ion_lsf, 'READ', lsf_num, s )
      call lsf_set_pc( lsf_num,
     *                 ion_epoch,ion_ra(1),ion_dec(1), ion_source(1), s)

      call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
      call enq_freq( sf_lun, freq, s )
      rpwl2dpkm   = (freq*1000.0D+0)/(const_c*const_d2r)
      sa2dpkm     = const_sa2r*const_2pi*rpwl2dpkm
      search_posn = map_size/2+1

C     ****************************************************************
C
C         Main Code
C         ---------
C

C     Get the sampling interval
      disp_rad = min( map_size/2*arcsec_per_gp, 2*search_rad )
      call io_getr( 'Display radius in arcsec ?', '*', disp_rad, s )
      plot_start= map_size/2+1 - int(disp_rad/arcsec_per_gp)-1
      plot_stop = map_size/2+1 + int(disp_rad/arcsec_per_gp)+1
      plot_size = plot_stop-plot_start + 1

      do 100, i = plot_start, plot_stop
          index(i) = (i-(map_size/2+1))*arcsec_per_gp
  100 continue

      call lsf_get_range( lsf_num, first_buff, last_buff, s )
      call io_geti( 'Sampling rate ?', '1', samp_rate, s )

      call pgbegin( 0, plot_device, 1, 1 )

      buff_num = first_buff
  200 if ((buff_num .gt. last_buff) .or. (s .ne. 0)) goto 400
          call ion_transform(lsf_num,
     *                       buff_num,
     *                       aperture,
     *                       plot_start, plot_stop,
     *                       s                               )

          call ion_calc_corr( map, search_posn, ion, s )
          if (s .eq. NO_VISDATA) s = 0

          if (s .eq. 0) then
C             Do plotting
              call pmadvance( s )
              if (s .ne. 0) then
                  call pgend
                  goto 9999
              end if

              call pgbbuf
C             Plot graph
              call pgvport( 0.1, 0.9, 0.4, 0.8 )
              call plot_data( plot_size,
     *                        index(plot_start), map(plot_start),
     *                        sa2dpkm, 1.0, s )
              call pglabel( 'Position (arcsec)', 'Flux (Jy/beam)',
     *                      'Correction (deg/km)'                  )

C             Draw bars for the search area.
              call pgqwin( start, stop, min_plot, max_plot )
              call pgmove( -search_rad*sa2dpkm, min_plot )
              call pgdraw( -search_rad*sa2dpkm, max_plot )
              call pgmove(  search_rad*sa2dpkm, min_plot )
              call pgdraw(  search_rad*sa2dpkm, max_plot )

C             Draw bar showing correction
              call pgmove( real(ion(1)*rpwl2dpkm), min_plot )
              call pgdraw( real(ion(1)*rpwl2dpkm), max_plot )

C             Print title
              call pgvport( 0.1, 0.9, 0.85, 1.0 )
              call pgwindow( 0.0, 100.0, 4.2, -0.2 )
              call lsf_title( lsf_num, 'All of LSF',buff_num,-1,title,s)
              call pgscf( 0 )
              do 300, i = 1, 4
                  call pgtext( 0.0, real(i), title(i) )
  300         continue
              call pgscf( 1 )
              call pgebuf
          else
              goto 9999
          end if
          buff_num = buff_num + samp_rate
      goto 200
  400 continue

      call pgend
      if ( s .ne. 0 ) goto 9999
      call lsf_close( lsf_num, s )

      if (s .eq. 0) return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .eq. usr_break) then
              s = 0
              call lsf_close( lsf_num, s )
          else
              call ion_wrerr( s, ' in subroutine ion_disp_samp ' )
          end if
          return
      end
