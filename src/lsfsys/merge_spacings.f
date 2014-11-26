C+merge_spacings
C
      SUBROUTINE merge_spacings(  lsf_num,
     *                            true_data,
     *                            plot_device,
     *                            s                      )

C
C     Plots merged spacings on the plot device
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
C     for merged spacings.
C    last edit: 4 Aug 2005  GP
C-
C=======================================================================
C
C     Function declarations.

      include        '/mrao/include/chrlib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/merge_types.inc'

C
C     Local constant declarations.
C         The maximum number of points that can be plotted
C         This is chosen to reflect the available buffer size
              integer         max_plot
              parameter     ( max_plot = max_aes*max_samp/2 )

C
C     Variables, equivalences and commons
C         General purpose loop counter
              integer         i
C         Text output device
              integer         out
C         LSF sample file unit number
              integer         sf_lun
C         Spacing list and string descriptor and number of spacings.
              integer         sp_list(max_vis)
              integer         num_spac
              character*80    list
C         Sidereal times for each sample
              integer         sid
              real            time( max_samp )
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         List of radial coordinates returned from lsf_get_radii
              real            radii( max_vis )
C         Merge type
              integer         merge_type
C         Merge list description - returned from set_merge_list
              integer         merge_list(2*max_vis)
              integer         no_groups
              integer         group_size(max_vis)
C         Current merge group number
              integer         group_num
C         Merged visibilities and a pointer to the first visibility
C         for the current sample in this array
              complex         merge_vis( max_plot )
              integer         vis_ptr
C         Number of LSF buffers to plot
              integer         num_buff
C         First, current and last LSF buffer number to display.
              integer         first_buff, buff_num, last_buff
C         The sampling rate to use and the minimum allowable samp rate.
              integer         samp_rate
              integer         min_rate
C         Plot title and line length in title.
              character*(80)  title(4)
              integer         lt
C         Plot type for plot_complex
              integer         plot_type

C    Place data on the work array
              include '/mrao/post/include/post_work_array.inc'
              equivalence (post_work(1),                    sp_list)
              equivalence (post_work(max_vis+1),            radii)
              equivalence (post_work(2*max_vis+1),          vis_list)
              equivalence (post_work(4*max_vis+1),          time)
              equivalence (post_work(4*max_vis+max_samp+1), merge_list)
              equivalence (post_work(6*max_vis+max_samp+1), group_size)
              equivalence (post_work(7*max_vis+max_samp+1), merge_vis)


C Subroutine initialisation
C -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call io_enqout( out )
      call lsf_enq_sf( lsf_num, sf_lun, i, s )
      plot_type = 0

C
C Main Code
C ---------
C
      merge_type = total_merge
      call get_merge( sf_lun, 'Merge type : ', '*', merge_type, s )
      call get_spacings(sf_lun, 'Spacing list : ', 'All',
     *                   list, sp_list, max_vis, num_spac, s)
      call lsf_set_spacings(lsf_num, num_spac, sp_list, 2, s)
      call set_merge(sf_lun, sp_list, num_spac, merge_type,
     *                merge_list, no_groups, group_size, s)

  100 call lsf_get_range(lsf_num, first_buff, last_buff, s)
      if (s .ne. 0) goto 9999

      num_buff  = last_buff - first_buff + 1
      min_rate  = max0( 1, int(num_buff/int(max_plot/no_groups)+1) )
c     samp_rate = max0( min_rate, int((num_buff-1)/800+1) )
      samp_rate = min_rate
      call io_geti( 'Sampling rate : ', '*', samp_rate, s )
      samp_rate = max( 1, samp_rate )
      if ( s .ne. 0 ) goto 9999

      if (samp_rate.lt.min_rate .or. samp_rate.ge.num_buff) then
          samp_rate = min0((num_buff-1), max0(min_rate, samp_rate))
          write(out,'(X,A,I4)') 'Sampling rate adjusted to ', samp_rate
      end if

      num_buff = 0
      buff_num = first_buff
      vis_ptr  = 1

  200 if (buff_num .gt. last_buff) goto 300
          call lsf_set_buffer( lsf_num, buff_num, s )
          if (true_data) then
              call lsf_get_vis(  lsf_num, max_vis,vis_list,num_spac, s)
          else
              call lsf_get_model(lsf_num, max_vis,vis_list,num_spac, s)
          end if

          call merge_vis_buffer( vis_list, num_spac,
     *                           merge_list, no_groups, group_size,
     *                           merge_vis(vis_ptr), s           )
          call lsf_get_sid(   lsf_num, sid, s )
          if ( s .ne. 0 ) goto 9999

          num_buff       = num_buff + 1
          buff_num       = buff_num + samp_rate
          vis_ptr        = vis_ptr  + no_groups
          time(num_buff) = float( sid )/36000.0
      goto 200
  300 continue

C     Make plot title
      call lsf_title( lsf_num, list, first_buff, last_buff, title, s )

      group_num = 1
      call plot_begin( plot_device, s )
  400 if ((s .ne. 0) .or. (group_num .gt. no_groups)) goto 1000
          if (group_size(group_num) .ne. 0) then
C             Update title for this spacing.
              call enq_grp_desc( sf_lun, merge_type, sp_list, num_spac,
     *                           merge_list, no_groups, group_size,
     *                           group_num, title(2), s )
              lt = chr_lenb(title(2))+1
              title(2)(lt:) = ' from '//list
c             write(title(2)(lt:),'(A,A)') ' from ', list

              call plot_complexN(  title,
     *                            'Visibilities vs time',
     *                            'ST (hrs)',
     *                            merge_vis(group_num),
     *                            num_buff, no_groups,
     *                            time,
     *                            plot_device,
     *                            plot_type,
     *                            lsf_num, s)
          end if
          group_num = group_num + 1
      goto 400
 1000 continue

      call pgend
      if ( s .ne. 0 ) goto 9999

      return

C
C Error Handling
C --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine MERGE_SPACINGS' )
          end if
          return
      end
