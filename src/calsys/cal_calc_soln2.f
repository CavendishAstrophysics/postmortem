C
C+ cal_calc_soln2

       subroutine cal_calc_soln2( m, list, vis, n, gains, s )
C      ------------------------------------------------------
C
C Solve for telescope based gains given visibilities and aerial matrix
C
C Given:
C    number of visibilities in list
       integer       m
C    list of visibilities to use
       integer       list(m)
C    visibility list
       complex       vis(*)
C    number of aerials
       integer       n
C
C Returned:
C    complex gain solutions
       complex       gains(n)
C    error status
       integer       s
C
C The least squares solution of the problems:
C
C       aerial_list(i,j).aerial_phase(i) = vis_phase(j)
C       aerial_list(i,j).aerial_log_amplitude(i) = vis_log_amplitude(j)
C
C is performed using the NAG routine F04QAF.
C
C The routine assumes that all the correlations between the antennas
C for which a solution is required are present.  This restriction is
C imposed by the method CAL_FIDDLE uses to ensure the closure
C quantities are zero and not factors of 2pi -- this restriction can
C be lifted if it is thought desirable to solve for CLFST phases using
C this routine.
C
C This routine must be preceeded by a call to cal_init_soln to define
C matrices to use in the solution.
*-

C global constants
       include '/mrao/post/include/global_constants.inc'

C local variables
C    NAG error return
       integer        status_NAG
C    string to hold error text
       character      string*30
C    phases and amplitudes for aerials and visibilities
       real*8         aerial_phase(max_aes), aerial_amp(max_aes)
       real*8         vis_phase(max_vis),    vis_amp(max_vis)
       real*8         errors_phase(max_aes), errors_amp(max_aes)
C    phase of reference antenna
       real*8         reference_phase
C    counters
       integer        im, in
C    work space array used by NAG
       integer        i_work
       parameter     (i_work = 100)
       real*8         work_space(i_work)
C    work space not used
       integer        i_local_work(1)
       real*8         r_local_work(1)
C    work space for closure calculations
       real*8         closure_phase(60)
       integer        num_closure
C variables used in passing information to local matrix routine
       include      '/mrao/post/include/cal_ae_matrix.inc'
       include      '/mrao/post/include/cal_control.inc'
       include      '/mrao/include/iolib_functions.inc'
       external      cal_ae_matrix

C error definitions
       include      '/mrao/post/include/calib_errors.inc'

C check status on entry
       if (s.ne.0) return

C check for call to cal_init_soln
       if (.not.matrix_initialised) then
         s = ill_matinit
         goto 999
       end if

C initialise visibility data
       do im=1,m
         vis_amp(im)   = log(abs(vis(list(im))))
         vis_phase(im) = atan2(imag(vis(list(im))),real(vis(list(im))))
       end do

C initialise solution
       do in=1,n
         aerial_phase(in) = 0.0D+0
         aerial_amp(in)   = 0.0D+0
       end do

C remove ambiguities of +/- Npi in visibility phases
       call cal_fiddle_phase( vis_phase, m, aerial_phase, n, s )

C report closure errors
       call cal_closure(n,m,vis_phase,mat,print_closure,ae_list,
     *                  num_closure,closure_phase,s)

C initialise solution
       do in=1,n
         aerial_phase(in) = 0.0D+0
         aerial_amp(in)   = 0.0D+0
       end do

C call NAG routine
       status_NAG = 1
       calculate_amp_solution = .false.
       call f04qaf( m, n, vis_phase, aerial_phase, errors_phase,
     *              cal_ae_matrix,
     *              0.0D+0, atol, btol_phase, 0.0D+0, 5*n, icontrol,
     *              its_done(1), anorm(1), acond(1),
     *              rnorm(1), arnorm(1), xnorm(1),
     *              work_space, r_local_work, 1, i_local_work, 1,
     *              inform(1), status_NAG                           )
       if (status_NAG.ne.0) then
         s = ill_calNAG
         goto 999
       end if
       status_NAG = 1
       calculate_amp_solution = .true.
       call f04qaf( m, n, vis_amp, aerial_amp, errors_amp,
     *              cal_ae_matrix,
     *              0.0D+0, atol, btol_amp, 0.0D+0, 5*n, icontrol,
     *              its_done(2), anorm(2), acond(2),
     *              rnorm(2), arnorm(2), xnorm(2),
     *              work_space, r_local_work, 1, i_local_work, 1,
     *              inform(2), status_NAG                           )
       if (status_NAG.ne.0) then
         s = ill_calNAG
         goto 999
       end if

C report results of the solution if required
       call cal_soln_report( print_report, s )

C set reference antenna to have zero phase
       reference_phase = aerial_phase(refant)
       if (refant.ne.0) then
         do in=1,n
           aerial_phase(in) = aerial_phase(in) - reference_phase
         end do
       end if

C define gain solutions
       do in=1,n
         gains(in) = cmplx( exp(aerial_amp(in))*cos(aerial_phase(in)),
     *                      exp(aerial_amp(in))*sin(aerial_phase(in))
     *                    )
         if (gains(in).ne.cmplx(0.0,0.0)) then
           gains(in) = cmplx( 1.0, 0.0 ) / gains(in)
         end if
       end do

C Error handling
999    continue
C .. test for error from NAG routine
       if (s.ne.0) call cal_wrerr(s,'in subroutine cal_calc_soln2')
       if (status_NAG.ne.0) then
         write(string,'(A,i2)')
     *         '*** NAG Error return code = ',status_NAG
         call cal_wrerr(s,string)
       end if

       end
