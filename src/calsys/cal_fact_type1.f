C+cal_fact_type1
C
      SUBROUTINE cal_fact_type1(  sf_lun,
     *                            num_vis,
     *                            sp_list,
     *                            vis,
     *                            mod_vis,
     *                            merge_type,
     *                            gains,
     *                            s               )

C     Sets up gain corrections for the given sample file.
C
C     Given:
C         Logical unit number of sample file
              integer         sf_lun
C         Number of visibilities in visibility buffer.
              integer         num_vis
C         Spacing list
              integer         sp_list(num_vis)
C         Visibility buffer and model visibility buffer.
              complex         vis(num_vis), mod_vis(num_vis)
C         Merge type - must be aerial_merge or hut_merge
              integer         merge_type

C     Returned:
C         Gain correction buffer.
              complex         gains(num_vis)
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/post/include/clfst_constants.inc'
      include        '/mrao/post/include/merge_types.inc'
      include        '/mrao/post/include/calib_errors.inc'

C
C     Local variables, equivilances and commons
C         Loop control variables and counters
              integer         i, grp_num, vis_num, vis_ptr
C         Merge control spacing list
              integer         merge_list( 2*max_spac )
C         Number of groups and size of each group in merge list.
              integer         num_grps, grp_size( max_aes )
C         Number of eastern merge groups
              integer         neast_grps
C         Merged model and true visibility buffers
*             complex         mod_grp_vis(max_aes),
              complex         grp_vis(max_aes)
C         Gain for each merge group
              complex         grp_gain(max_aes)

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if (merge_type .eq. aerial_merge) then
          neast_grps = neast_aes
      else if (merge_type .eq. hut_merge) then
          neast_grps = neast_huts
      end if

C
C     Main Code
C     ---------
C
      if (merge_type .eq. no_merge) then
          do 100, i = 1, num_vis
              if (vis(i) .ne. (0.0,0.0)) then
                  gains(i) = mod_vis(i)/vis(i)
              else
                  gains(i) = (1.0, 0.0)
              end if
  100     continue
      else if ((merge_type .eq. aerial_merge) .or.
     *         (merge_type .eq. hut_merge)         ) then
C         Valid merge type - get the merge list
          call set_merge( sf_lun, sp_list, num_vis, merge_type,
     *                    merge_list, num_grps, grp_size, s )

C         Initialise the visibility gain corrections and normalise
C         to a point source.
          do 200, i = 1, num_vis
              gains(i) = (1.0,0.0)
              vis(i)   = vis(i)/mod_vis(i)
  200     continue

C         Merge the visibilities
          call merge_vis_buffer( vis, num_vis, merge_list,
     *                           num_grps, grp_size, grp_vis, s )

C         Calculate the East group corrections.
          i = 0
          do 400, grp_num = 1, neast_grps
              if (grp_vis(grp_num) .ne. (0.0,0.0)) then
                  grp_gain(grp_num) = cmplx(1.0,0.0)/grp_vis(grp_num)
              else
                  grp_gain(grp_num) = (1.0,0.0)
              end if

              do 300, vis_num = 1, grp_size(grp_num)
                  i = i+1
                  vis_ptr = merge_list(i)
                  vis(vis_ptr)   = vis(vis_ptr)   * grp_gain(grp_num)
                  gains(vis_ptr) = gains(vis_ptr) * grp_gain(grp_num)
  300         continue
  400     continue

C         Merge the visibility buffer with the east group corrections.
          call merge_vis_buffer( vis, num_vis, merge_list,
     *                           num_grps, grp_size, grp_vis, s )

C         Calculate the west group corrections.
          do 600, grp_num = neast_grps+1, num_grps
              if (grp_vis(grp_num) .ne. (0.0,0.0)) then
                  grp_gain(grp_num) = cmplx(1.0,0.0)/grp_vis(grp_num)
              else
                  grp_gain(grp_num) = (1.0,0.0)
              end if

              do 500, vis_num = 1, grp_size(grp_num)
                  i = i+1
                  vis_ptr = merge_list(i)
                  vis(vis_ptr)   = vis(vis_ptr)   * grp_gain(grp_num)
                  gains(vis_ptr) = gains(vis_ptr) * grp_gain(grp_num)
  500         continue
  600     continue
      else
          s = ILL_CALIBRATION
      end if

      if (s .ne. 0) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr( s, 'in subroutine cal_fact_type1' )
          return
      end
