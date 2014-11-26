C
C+display_hist
C
      SUBROUTINE display_hist(   lsf_num,
     *                            plot_device,
     *                            s                      )

C
C     Plots histogram of samples on the plot device
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Runs through the current logical sample file accumulating  data
C     sample by sample.
C
C-
C=======================================================================
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/plot_control.inc'

C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         Spacing list and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         Min/max value in histogram
              real            minmax
C         Max y value for histogram
              real            ymax
C         Maximum number of bins in the histogram
              integer         max_bins
              parameter       (max_bins = 256)
C         Actual number of bins
              integer         n_bins
C         Bin size
              real            bin_size
              real            phase_bin_size
C         Data binned into bins
              real            plot1_data(max_bins)
              real            plot2_data(max_bins)
C         Array for pgbin
              real            x(max_bins)
C         Helpful variables
              real            data_real, data_imag, data_amp, data_phase
C         Useful index
              integer         ind
C         Number of spacings to plot
              integer         num_spac
C         Current and last LSF buffer to display
              integer         buff_num, last_buff
C         Plot type for plot_complex
              integer         plot_type

C    Place data in work space
              include '/mrao/post/include/post_work_array.inc'
              equivalence (post_work(1),           sp_list)
              equivalence (post_work(max_vis+1),  vis_list)
              equivalence (post_work(2*max_vis+1), plot1_data)
              equivalence (post_work(2*max_vis+max_bins+1), plot2_data)

C
C Subroutine initialisation
C -------------------------

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      plot_type = plot_data_type
      call lsf_enq_sf( lsf_num, sf_lun, i, s )


C Main Code
C ---------
c
      call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
      call lsf_get_range( lsf_num, buff_num, last_buff, s )
      call io_getr( 'Min/max of data :', '0.5', minmax, s)
      call io_geti( 'Number of bins :', '128', n_bins, s)
      call pgbegin( 0, plot_device, 1, 1 )
      call pgask(.true.)
      call pgbbuf

      do 125 i=1,max_bins
          plot1_data(i) = 0
          plot2_data(i) = 0
  125 continue
      if (plot_type.eq.plot_cossin) then
          bin_size = 2 * minmax / n_bins
      else
          bin_size = 1.5 * minmax / n_bins
          phase_bin_size = 6.283 / n_bins
      end if

  100 if ((s .ne. 0) .or. (buff_num .gt. last_buff-1)) goto 1000
          call lsf_set_buffer( lsf_num, buff_num, s )
          call lsf_get_vis( lsf_num, max_vis, vis_list, num_spac, s )

          do 150, i = 1, num_spac
              data_real = real(vis_list(i))
              data_imag = aimag(vis_list(i))
              if (plot_type.eq.plot_cossin) then
                    ind = int((data_real + minmax) / bin_size)
                    if ((ind.gt.0) .and. (ind.le.n_bins)) then
                        plot1_data(ind) = plot1_data(ind) + 1
                    end if
                    ind = int((data_imag + minmax) / bin_size)
                    if ((ind.gt.0) .and. (ind.le.n_bins)) then
                        plot2_data(ind) = plot2_data(ind) + 1
                  end if
              else
                  data_amp=sqrt(data_real*data_real+data_imag*data_imag)
                   if (data_real .ne. 0) then
                        data_phase = atan(data_imag/data_real)
                   else
                        data_phase = 0
                   end if
                   if ((data_real.lt.0).and.(data_imag.lt.0)) then
                      data_phase = data_phase - 3.141
                   else if ((data_real.lt.0).and.(data_imag.gt.0)) then
                      data_phase = data_phase + 3.141
                   end if
                   ind = int(data_amp / bin_size)
                   if ((ind.gt.0) .and. (ind.le.n_bins)) then
                       plot1_data(ind) = plot1_data(ind) + 1
                   end if
                   ind = int((data_phase + 3.141) / phase_bin_size)
                   if ((ind.gt.0) .and. (ind.le.n_bins)) then
                        plot2_data(ind) = plot2_data(ind) + 1
                   end if
              end if
  150     continue
          buff_num = buff_num+1
      goto 100
c
 1000 if (s.ne.0) goto 2000

      if (plot_type.ne.plot_cossin) goto 3000

      do 50, i = 1,n_bins
          x(i) = -minmax + bin_size * (i-1)
  50  continue

      ymax = 0
      do 200 i=1,n_bins
          if (ymax.lt.plot1_data(i)) ymax = plot1_data(i)
  200 continue
      call pgenv(-minmax, minmax, 0, ymax*1.1, 0, 0)
      call pgbin(n_bins, x, plot1_data, .false.)
      call pglabel('Cos values','Frequency','Histogram of cos values')
      call pgebuf
      call pgbbuf
      call pmadvance( s )

      ymax = 0
      do 250 i=1,n_bins
          if (ymax.lt.plot2_data(i)) ymax = plot2_data(i)
  250 continue
      call pgenv(-minmax, minmax, 0, ymax*1.1, 0, 0)
      call pgbin(n_bins, x, plot2_data, .false.)
      call pglabel('Sin values','Frequency','Histogram of sin values')
      call pgebuf
      call pgbbuf

      goto 2000

 3000 continue
      do 350, i = 1,n_bins
          x(i) = bin_size * (i-1)
  350 continue

      ymax = 0
      do 300 i=1,n_bins
       if (ymax.lt.plot1_data(i)) ymax = plot1_data(i)
  300 continue

      call pgenv(0, minmax*1.5, 0, ymax*1.1, 0, 0)
      call pgbin(n_bins, x, plot1_data, .false.)
      call pglabel('Amplitude values','Frequency',
     & 'Histogram of amplitudes')
      call pgebuf
      call pgbbuf
      call pmadvance( s )

      do 400 i=1,n_bins
        x(i) = -3.142 + (phase_bin_size * (i-1))
  400 continue

      ymax = 0
      do 450 i=1,n_bins
       if (ymax.lt.plot1_data(i)) ymax = plot1_data(i)
  450 continue

      call pgenv(-3.142, 3.142, 0, ymax, 0, 0)
      call pgbin(n_bins, x, plot2_data, .false.)
      call pglabel('Phase values','Frequency','Histogram of phases')
      call pgebuf
      call pgbbuf

 2000 continue
      call pgebuf
      call pgend
      if ( s .ne. 0 ) goto 9999

      return


C Error Handling
C --------------

 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine DISPLAY_hist' )
          end if
          return
      end
