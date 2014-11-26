C
C+display_spacing
C
      SUBROUTINE display_spacing( lsf_num,
     *                            true_data,
     *                            plot_device,
     *                            s                      )

C
C     Plots spacings on the plot device
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Flag set .true. if real (not model) data is to be plotted
              logical             true_data
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Runs through the current logical sample file displaying data
C     spacing by spacing.
C
C
C
C-
C=======================================================================
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'

C
C     Local constant and variable declarations
C
C     Constants
C         The maximum number of spacings that can be plotted.
              integer         max_sp_plot
              parameter     ( max_sp_plot = 40 )

C     Variables, equivalences and commons
C         General purpose loop counter
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         output unit
              integer         out
C         Spacing list and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         Radii of each spacing.
              real            radii( max_vis )
C         Sidereal times for each sample
              integer         sid
              real            time( max_samp )
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_sp_plot, max_samp )
C         Number of LSF buffers to plot
              integer         num_buff
C         First, current and last LSF buffer number to display.
              integer         first_buff, buff_num, last_buff
C         The sampling rate to use.
              integer         samp_rate
C         Plot title
              character*(80)  title(4)
C         The number of spacings in the current LSF
              integer         num_spac
C         Current spacing, number of spacings in buffer and counter
              integer         spac_num, spac_curr, ispac
C         Plot type for plot_complex
              integer         plot_type
C         Spacing designation
              integer         isp, iba, ich, iae1, iae2
              character*1     sub_bands(5)

C    Place data in work space
              include '/mrao/post/include/post_work_array.inc'
              equivalence (post_work(1),           sp_list)
              equivalence (post_work(max_vis+1),   radii)
              equivalence (post_work(2*max_vis+1), vis_list)
              equivalence
     *         (post_work(2*max_vis+2*max_sp_plot*max_samp+1),time)


C    Data
              data     sub_bands  / 'A', 'B', 'C', 'D', 'E' /


C Subroutine initialisation
C -------------------------

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      plot_type = 0
      call lsf_enq_sf( lsf_num, sf_lun, i, s )
      call io_enqout( out )

C Main Code
C ---------

      call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac,
     *                       sp_list(1), 2, s )
50    call lsf_get_range( lsf_num, first_buff, last_buff, s )
      samp_rate = 1
      call io_geti( 'Sampling rate : ', '*', samp_rate, s )
      if (((last_buff - first_buff)/samp_rate)+ 1 .gt. max_samp ) then
          write (out, *) ' More than ',max_samp,' samples'
          goto 50
      endif
      samp_rate = max( 1, samp_rate )
      if ( s .ne. 0 ) goto 9999

C Make plot title and open plot device
      call lsf_title( lsf_num, list, first_buff, last_buff, title, s )
      call plot_begin( plot_device, s )

C Loop, fill visibility buffer and plot
      spac_num = 1
      do while ( spac_num.le.num_spac )

C .. find number of spacings for this buffer
        spac_curr = min( max_sp_plot, num_spac-spac_num+1 )
C .. set current spacing list for the visibility buffer
        call lsf_set_spacings( lsf_num, spac_curr,
     *                         sp_list(spac_num), 2, s )
C .. fill current buffer
        num_buff = 0
        buff_num = first_buff
  200   if (buff_num .gt. last_buff) goto 300
          num_buff = num_buff + 1
          call lsf_set_buffer( lsf_num, buff_num, s )
          if (true_data) then
              call lsf_get_vis(   lsf_num,
     *                            max_sp_plot,
     *                            vis_list( 1, num_buff ),
     *                            spac_curr,
     *                            s           )
          else
              call lsf_get_model( lsf_num,
     *                            max_sp_plot,
     *                            vis_list( 1, num_buff ),
     *                            spac_curr,
     *                            s           )
          end if

          call lsf_get_sid(   lsf_num, sid, s )
          if ( s .ne. 0 ) goto 9999
          time(num_buff) = float( sid )/36000.0
          buff_num = buff_num + samp_rate
          goto 200
  300   continue
        call lsf_get_radii( lsf_num, max_sp_plot, radii, spac_curr, s )

        do ispac = 1,spac_curr
C ... Update title for this spacing.
          call lsf_enq_desig( lsf_num, ispac, isp, iba, ich, s )
          call lsf_enq_ae_vis( lsf_num, ispac, iae1, iae2, s )
          title(2) = ' '
          write( title(2),
     *          '(A,I4,'':'',A1,'':'',I1,A,F8.1,A,I2,A,I2)' )
     *            ' Spacing ', isp,sub_bands(iba),ich,'; ',
     *            radii(ispac),'/\; Ae ',iae1,'/',iae2

          call plot_complexN(  title,
     *                        'Visibilities vs time',
     *                        'ST (hrs)',
     *                        vis_list(ispac,1),
     *                        num_buff, max_sp_plot,
     *                        time,
     *                        plot_device,
     *                        plot_type,
     *                        lsf_num, s)

        end do

C .. increment pointer in full spacing list
        spac_curr = min( max_sp_plot, num_spac-spac_num+1 )
        spac_num = spac_num + spac_curr

      end do

1000  call pgend
      if ( s .ne. 0 ) goto 9999

      return


C Error Handling
C --------------

 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine DISPLAY_SPACING' )
          end if
          return
      end
