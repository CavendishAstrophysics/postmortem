C+ cal_fact_type3
C
      subroutine cal_fact_type3( sf_lun, num_vis, vis_list, vis,
     *                            mod_vis, merge_type, gains, s  )
C     ------------------------------------------------------------

C Sets up gain corrections for the given sample file.
C
C Given:
C   Logical unit number of sample file
      integer         sf_lun
C   Number of visibilities in visibility buffer.
      integer         num_vis
C   Visibility list
      integer         vis_list(num_vis)
C   Visibility buffer and model visibility buffer.
      complex         vis(num_vis), mod_vis(num_vis)
C   Merge type - must be aerial_merge or hut_merge
      integer         merge_type

C Returned:
C   Gain correction buffer.
      complex         gains(num_vis)
C   Status variable - must be zero on entry - otherwise error
      integer         s
C
*-
C Global includes
      include        '/mrao/post/include/global_constants.inc'
      include        '/mrao/post/include/cal_solution.inc'
      include        '/mrao/post/include/cal_ae_matrix.inc'
      include        '/mrao/post/include/merge_types.inc'
      include        '/mrao/post/include/calib_errors.inc'

C
C Local variables, equivilances and commons
C   Loop control variables and counters
       integer         i, ii, n, nn, grp_num, iv
C   Merge control spacing list
       integer         merge_list( 2*max_vis )
C         Number of groups and size of each group in merge list.
       integer         num_grps, grp_size( max_vis )
C   Merged visibility buffers
       complex         grp_vis(max_vis), grp_gain

C Check for non zero entry status
       if ( s .ne. 0 ) return

C set up merge
       call set_merge( sf_lun, vis_list, num_vis, merge_type,
     *                 merge_list, num_grps, grp_size, s )

C initialise the visibility gain corrections and normalise to a point source
       do i = 1, num_vis
         gains(i) = (1.0,0.0)
         vis(i)   = vis(i)/mod_vis(i)
       end do
       do i = 1, max_vis
         vis_gains(i) = (1.0,0.0)
       end do
C initialise the solution
       do n = 1,max_RT_aes
         do i = 1,max_subb
           do ii=1,max_channel
             ae_gains(ii,i,n) = cmplx(1.0,0.0)
           end do
         end do
       end do

C merge the visibilities
       call merge_vis_buffer( vis, num_vis, merge_list,
     *                        num_grps, grp_size, grp_vis, s )

C determine gains for each group and relate these to visibility lists
       ii = 0
       do grp_num = 1,num_grps
         if (grp_vis(grp_num).ne.(0.0,0.0)) then
           grp_gain = (1.0,0.0)/grp_vis(grp_num)
         else
           grp_gain = (1.0,0.0)
         end if
         do nn=1,grp_size(grp_num)
           ii = ii + 1
           gains(merge_list(ii)) = grp_gain
           iv = vis_list(merge_list(ii))
           vis_gains(iv) = grp_gain
         end do
       end do

C record the solution being found for this sample file
       current_solution = .true.

C error Handling
 999   continue
       if (s.ne.0)  then
          call cal_wrerr( s, 'in subroutine cal_fact_type3' )
       end if

       end
