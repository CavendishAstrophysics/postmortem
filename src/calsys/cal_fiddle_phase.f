

C+ cal_fiddle_phase

       subroutine cal_fiddle_phase( vis_phase, nv, ae_phase, na, s )
C
C      Remove ambiguities of 2Npi in visibility phases
C
C Given:
C    visibility phases (radians)
       real*8     vis_phase(*)
C    numb3er of visbility phases
       integer    nv
C Work-Space:
C    aerial phases
       real*8     ae_phase(*)
C    number of aerials
       integer    na
C Returned:
C    status value on exit
       integer    s
C
C Modify the visibility phases to remove ambiguities of 2Npi
C-

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/cal_ae_matrix.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/include/constants.inc'

C counters
       integer    n
C test value
       integer    itest, test_ae_phase(max_RT_aes), test_count

C check status on entry
       if ( s.ne.0 ) return

C initialise aerial phase array
       do n=1,na
         ae_phase(n) = 0.0D+0
         test_ae_phase(n) = 0
       end do

C find initial estimates of aerial phases
       test_count = 0
       do n=1,nv
         if (mat(1,n).eq.refant) then
           ae_phase(mat(2,n)) = -vis_phase(n)
           test_ae_phase(mat(2,n)) = 1
           test_count = test_count + 1
         else if (mat(2,n).eq.refant) then
           ae_phase(mat(1,n)) = vis_phase(n)
           test_ae_phase(mat(1,n)) = 1
           test_count = test_count + 1
         end if
       end do
       ae_phase(refant) = 0.0
       test_ae_phase(refant) = 1
       test_count = test_count + 1

C remove ambiguities based on these estimates of aerial phase
       do n=1,nv
         if ( test_ae_phase(mat(1,n)).eq.1 .and.
     *        test_ae_phase(mat(2,n)).eq.1 ) then
           itest = nint(
     *           (ae_phase(mat(1,n))-ae_phase(mat(2,n))-vis_phase(n))/
     *                            const_2pi
     *                 )
           vis_phase(n) = vis_phase(n) + float(itest)*const_2pi
         end if
       end do

       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_fiddle_phase' )

       end
