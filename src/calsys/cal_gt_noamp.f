C
C
C+cal_gt_noamp

       subroutine cal_gt_noamp( s )
C      ----------------------------
C
C Reset the current gains table so the amplitudes are unity
C
C      Returned:
C         error status
                  integer      s
C
C Reset the current gains table so that the amplitude correction factors
C are set to unity.
C
C PA 24/05/90
C-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_solution.inc'
       include '/mrao/post/include/cal_common.inc'


C Local variables
C    counters
       integer     iae, isb, ich, i


       if (s.ne.0) return

       do i=1,max_vis
         vis_gains(i) = vis_gains(i) / cmplx(cabs(vis_gains(i)),0.0)
       end do
       do iae=1,max_RT_aes
         do isb=1,max_subb
           do ich=1,max_channel
             ae_gains(ich,isb,iae) = ae_gains(ich,isb,iae) /
     *                      cmplx(cabs(ae_gains(ich,isb,iae)),0.0)
           end do
         end do
       end do

       if (s.ne.0) call cal_wrerr(s,'in cal_gt_noamp')

       end
