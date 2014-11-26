C+ cal_fact_type2
C
      subroutine cal_fact_type2(  sf_lun, num_vis, vis_list, vis,
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
       integer         i, ii, n, nn, grp_num
       integer         iv, il, il1, iv_list(max_vis)
       integer         iae1, iae2
C   Aerial Matrix
       integer*2       ia(2,max_aes)
C   Merge control spacing list
       integer         merge_list( 2*max_vis )
C         Number of groups and size of each group in merge list.
       integer         num_grps, grp_size( max_vis )
C   Merged visibility buffers
       complex         grp_vis(max_vis)
C   Gains for each logical aerial
       complex         actual_gains(max_aes)
C   Telescope description
       integer         nae, nsp, nsb, nch, isp, isb, ich, isb0, ich0
       integer         logical_ae, actual_ae

C Check for non zero entry status
       if ( s .ne. 0 ) return

C enquire structure of the telescope
       call enq_obsdef ( sf_lun, nae, nsp, nsb, nch, s )

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

C merge the visibilities
       call merge_vis_buffer( vis, num_vis, merge_list,
     *                        num_grps, grp_size, grp_vis, s )

C set the number of logical aerials to solve for depending on merge type
       if (merge_type.eq.no_merge) then
         logical_ae = nsb*nch
       else if (merge_type.eq.channel_merge) then
         logical_ae = nsb
       elseif (merge_type.eq.subband_merge) then
         logical_ae = 1
       end if

C initialise the solution
       do n = 1,max_RT_aes
         do i = 1,max_subb
           do ii=1,max_channel
             ae_gains(ii,i,n) = cmplx(1.0,0.0)
           end do
         end do
       end do

C loop for each logical aerial
       do il=1,logical_ae

C define the telescope array
         ii = 1
         iv = 0
         do grp_num = 1,num_grps
           if (grp_vis(grp_num).ne.(0.0,0.0)) then
             nn = vis_list(merge_list(ii))
             call enq_ae_vis( sf_lun, nn, iae1, iae2, s )
             isp = ((nn-1)/(nch*nsb)) + 1
             isb = (nn - (isp-1)*nsb*nch - 1)/nch + 1
             ich = (nn - (isp-1)*nsb*nch - (isb-1)*nch)
             if (merge_type.eq.no_merge) then
               il1 = (isb-1)*nch + ich
             else if (merge_type.eq.channel_merge) then
               il1 = isb
             else
               il1 = 1
             end if
             if (il1.eq.il) then
               iv = iv + 1
               ia(1,iv) = iae1
               ia(2,iv) = iae2
               iv_list(iv) = grp_num
             end if
           end if
           ii = ii + grp_size(grp_num)
         end do

         if (iv.ge.1) then
           call cal_init_soln( iv, max_RT_aes, ia, actual_ae, s )

C ... find the solution for this logical aerial
           if (merge_type.eq.no_merge) then
              isb = (il-1)/nch + 1
              ich = il - (isb-1)*nch
              call enq_iba_code( sf_lun, isb, isb0, s)
              call enq_ich_code( sf_lun, ich, ich0, s)
              call cal_set_logical( isb0, ich0, s )
           else if (merge_type.eq.channel_merge) then
              call enq_iba_code( sf_lun, il, isb0, s)
              call cal_set_logical( isb0, 0, s )
           else
              call cal_set_logical( 0, 0, s )
           end if
           call cal_calc_soln2( iv, iv_list, grp_vis,
     *                          actual_ae, actual_gains, s )

C ... save solution for this logical aerial
           do n = 1,actual_ae
             if (merge_type.eq.no_merge) then
               ae_gains(ich0,isb0,ae_list(n)) = actual_gains(n)
             else if (merge_type.eq.channel_merge) then
               do ich = 1,nch
                 call enq_ich_code(sf_lun,ich,ich0,s)
                 ae_gains(ich0,isb0,ae_list(n)) = actual_gains(n)
               end do
             else
               do isb = 1,nsb
                 call enq_iba_code(sf_lun,isb,isb0,s)
                 do ich = 1,nch
                   call enq_ich_code(sf_lun,ich,ich0,s)
                   ae_gains(ich0,isb0,ae_list(n)) = actual_gains(n)
                 end do
               end do
             end if
           end do
         end if
       end do

C determine the gains for each visibility
       do iv=1,num_vis
         call enq_vis_desig( sf_lun, vis_list(iv), isp, isb, ich, s)
         call enq_ae_vis( sf_lun, vis_list(iv), iae1, iae2, s )
         gains(iv) =
     *        ae_gains(ich,isb,iae1)*conjg(ae_gains(ich,isb,iae2))
       end do

C record the solution being found for this sample file
       current_solution = .true.

C error Handling
 999   continue
       if (s.ne.0)  then
          call cal_wrerr( s, 'in subroutine cal_fact_type2' )
       end if

       end
