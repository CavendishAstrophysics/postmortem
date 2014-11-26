C
C
C+scan_samples
C
       subroutine scan_samples( sf_num, lsf_num,
     :                          sp_list, nspac, ibuff1, ibuff2,
     :                          merge_list, n_groups, group_size,
     :                          acc, acc_sq, n_samples, status    )
C      ------------------------------------------------------------
C
C Return statistics for the samples in a sample file
C
C Given:
C    Sample file logical unit number and logical sample file number
       integer      sf_num, lsf_num
C    Spacing list and length
       integer      sp_list(*), nspac
C    Buffer range
       integer      ibuff1, ibuff2
C    List, number and size of merge groups
       integer      merge_list(*), n_groups, group_size(*)
C
C Returned:
C    Sums of groups (cos, sin, ampl, phase) and sum of squares
       real*8       acc(4,*), acc_sq(4,*)
C    Number of samples contributing for each group
       integer      n_samples(*)
C    Status
       integer      status
C
C=======================================================================
C
C   Constants and global includes
C
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/constants.inc'
C
C   local variables
       integer      ig, i, ibuff
       complex      vis_list(max_vis), vis_merge(max_vis)
       real*4       rcos, rsin, ramp, rphi


C Main Code
C ---------

C check status on entry
       if (status.ne.0) return

C initialise working arrays for statistics
       do ig=1,n_groups
         n_samples(ig)=0
         do i=1,4
           acc(i,ig)=0.d0
           acc_sq(i,ig)=0.d0
         end do
       end do

C scan the sample file
       do ibuff=ibuff1,ibuff2
         call lsf_set_buffer( lsf_num, ibuff, status)
         call lsf_get_vis( lsf_num, max_vis, vis_list, nspac, status )
         call merge_vis_buffer( vis_list, nspac,
     :                          merge_list, n_groups, group_size,
     :                          vis_merge, status                    )
         if (status.eq.0) then

C for each group, accumulate cos, sin, amplitude and phase
           do ig=1,n_groups
             rphi=0.0
             rcos=real(vis_merge(ig))
             rsin=aimag(vis_merge(ig))
             ramp=cabs(vis_merge(ig))
             if (ramp.gt.0.0) then
               rphi=atan2(rsin,rcos)/const_d2r
               acc(1,ig)=acc(1,ig)+rcos
               acc(2,ig)=acc(2,ig)+rsin
               acc(3,ig)=acc(3,ig)+ramp
               acc(4,ig)=acc(4,ig)+rphi
               acc_sq(1,ig)=acc_sq(1,ig)+rcos*rcos
               acc_sq(2,ig)=acc_sq(2,ig)+rsin*rsin
               acc_sq(3,ig)=acc_sq(3,ig)+ramp*ramp
               acc_sq(4,ig)=acc_sq(4,ig)+rphi*rphi
               n_samples(ig)=n_samples(ig)+1
             endif
           enddo

         endif
       enddo

C Error Handling
C --------------

       if (status.ne.0) then
         call lsf_wrerr( status, 'in routine SCAN_SAMPLES' )
       end if

       end
