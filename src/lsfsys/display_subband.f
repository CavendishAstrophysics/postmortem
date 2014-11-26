C
C
C
C+display_subband
C
      subroutine display_subband( lsf_num,
     *                            true_data,
     *                            plot_device,
     *                            s                      )

C
C     Plots all channels for a given band on the same output page
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
C     visibility by visibility.  All channels belonging to the same
C     sub-band and spacing are displayed on the same output page.
C
C
C     PA, 24/5/90
C     PA, 17/6/91, bug fixed: previously did not handle redundant
C                  consecutive speacings in a spacing list
C-
C=======================================================================
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/iolib_functions.inc'
      include  '/mrao/include/chrlib_functions.inc'
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
C         Parameters defining current spacing and total no. channels
              integer         current_spacing, current_band, channels
              integer         current_iae1, current_iae2
C         Plot title
              character*(80)  title(4)
C         Plot mode
              character*(20)  mode
C         The number of spacings in the current LSF
              integer         num_spac
C         Current spacing, spacings in buffer and spacing counter
              integer         spac_num, spac_curr, ispac
C         Plot type for plot_complex
              integer         plot_type
C         Spacing designation
              integer         isp, iba, ich, iae1, iae2
              character*1     sub_bands(5)
C         Additional title
              character*80    extra_title

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
              equivalence (post_work(i_point_3), time)
              integer      i_point_4
              parameter   (i_point_4 = i_point_3 + max_samp)
              equivalence (post_work(i_point_4), vis_list)

C    Data
              data     sub_bands  / 'A', 'B', 'C', 'D', 'E' /


C Subroutine initialisation
C -------------------------

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      plot_type = 0
      call lsf_enq_sf( lsf_num, sf_lun, i, s )


C Main Code
C ---------

      call get_spacings( sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_get_range( lsf_num, first_buff, last_buff, s )
      samp_rate = 1
      call io_geti( 'Sampling rate : ', '*', samp_rate, s )
      samp_rate = max( 1, samp_rate )
      if ( s .ne. 0 ) goto 9999

C Make plot title and open plot device
      call lsf_title( lsf_num, list, first_buff, last_buff, title, s )
C define current spacing, band and sub-band
      spac_num = 1
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
      call lsf_enq_desig( lsf_num, spac_num, isp, iba, ich, s )
      call lsf_enq_ae_vis( lsf_num, spac_num, iae1, iae2, s )
      current_band    = iba
      current_spacing = isp
      current_iae1    = iae1
      current_iae2    = iae2
      channels = 1
      do i = 1,num_spac
        call lsf_enq_desig( lsf_num, i, isp, iba, ich, s )
        call lsf_enq_ae_vis( lsf_num, i, iae1, iae2, s )
        if (isp.ne.current_spacing .or. iba.ne.current_band  .or.
     *      iae1.ne.current_iae1 .or. iae2.ne.current_iae2   ) then
          goto 100
        end if
        channels = channels + 1
      end do
100   continue

C open pgplot device
      call plot_begin( plot_device, s )
      call plot_enqmode( mode, s )
      call plot_setmode( 'multiple', s )
      call plot_setpage( channels, s )


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
          if ( s .ne. 0 ) goto 1000
          time(num_buff) = float( sid )/36000.0
          buff_num = buff_num + samp_rate
          goto 200
  300   continue
        call lsf_get_radii( lsf_num, max_sp_plot, radii, spac_curr, s )

C .. loop through the spacing list
        do ispac = 1,spac_curr

C ... look for attention
          if (io_attn(s)) goto 1000
C ... find current spacing designation (sub-band, channel etc.)
          call lsf_enq_desig( lsf_num, ispac, isp, iba, ich, s )
C ... advance to new physical page if we move onto a new spacing/sub-band
          if ( (isp.ne.current_spacing) .or.
     *         (iba.ne.current_band   )     ) then
              continue
          end if
C ... construct title
          call lsf_enq_desig( lsf_num, ispac, isp, iba, ich, s )
          call lsf_enq_ae_vis( lsf_num, ispac, iae1, iae2, s )
          extra_title = ' '
          title(2) = ' '
          write( extra_title,
     *          '(A,I4,'':'',A1,A,I2,A,I2)' )
     *            ' Spacing ', isp,sub_bands(iba),
     *            '; Ae ',iae1,'/',iae2
           write( title(2),
     *          '(A,I2,''; '',F8.1,A)' )
c    *            ' Channel ', ich,radii(spac_num),'/\\'
     *            ' Channel ', ich,radii(ispac),'/\\'


C ... plot data or model
          call plot_complexN(  title,
     *                        extra_title,
     *                        'ST (hrs)',
     *                        vis_list(ispac,1),
     *                        num_buff, max_sp_plot,
     *                        time,
     *                        plot_device,
     *                        plot_type,
     *                        lsf_num, s)
        end do

C .. increment spacing pointer
        spac_curr = min( max_sp_plot, num_spac-spac_num+1 )
        spac_num = spac_num + spac_curr

      end do

C close pgplot device and reset plot mode
1000  if ( s.eq.usr_break ) s = 0
      call plot_setmode( mode(1:chr_lenb(mode)), s )
      call pgend
      if ( s .ne. 0 ) goto 9999

      return

C
C Error Handling
C --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine DISPLAY_SUBBAND' )
          end if
          return
      end
