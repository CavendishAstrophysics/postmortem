C
C
C
C+display_noise
C
      SUBROUTINE display_noise(   lsf_num,
     *                            plot_device,
     *                            s                      )

C
C     Plots sample to sample noise on the plot device
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
C     Runs through the current logical sample file displaying data
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

C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i
C         Sample file logical unit number
              integer         sf_lun
C         Plot title
              character*(80)  title(4)
C         Spacing list and string description.
              integer         sp_list( max_vis )
              character*(80)  list
C         Radii of each spacing.
              real            radii( max_vis )
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
              complex         vis_list2( max_vis )
C         Number of spacings to plot
              integer         num_spac
C         Current and last LSF buffer to display
              integer         buff_num, last_buff
C         Sampling rate in LSF buffers.
              integer         samp_rate
C         Plot type for plot_complex
              integer         plot_type

C    Place data in work space
              include '/mrao/post/include/post_work_array.inc'
              equivalence (post_work(1),           sp_list)
              equivalence (post_work(max_vis+1),   radii)
              equivalence (post_work(2*max_vis+1), vis_list)
              equivalence (post_work(4*max_vis+1), vis_list2)


C
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
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )
      call lsf_get_range( lsf_num, buff_num, last_buff, s )
      call io_geti( 'Sampling rate : ', '1', samp_rate, s )

      call plot_begin( plot_device, s )

  100 if ((s .ne. 0) .or. (buff_num .gt. last_buff-1)) goto 1000
          call lsf_set_buffer( lsf_num, buff_num, s )
          call lsf_get_vis( lsf_num, max_vis, vis_list, num_spac, s )
          call lsf_get_radii( lsf_num, max_vis, radii, num_spac, s )
          call lsf_set_buffer( lsf_num, buff_num+1, s )
          call lsf_get_vis( lsf_num, max_vis, vis_list2, num_spac, s )

          do 150, i = 1, num_spac
              vis_list(i) = vis_list(i) - vis_list2(i)
  150     continue

          call lsf_title( lsf_num, list, buff_num, -1, title, s )
          call plot_complexN(  title,
     *                        'Sample difference vs radius',
     *                        'Radius (wavelengths)',
     *                        vis_list,
     *                        num_spac, 1,
     *                        radii,
     *                        plot_device,
     *                        plot_type,
     *                        lsf_num, s)

          buff_num = buff_num+samp_rate
      goto 100
 1000 continue

      call pgend
      if ( s .ne. 0 ) goto 9999

      return


C Error Handling
C --------------

 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine DISPLAY_NOISE' )
          end if
          return
      end
