
C+noise
C
      SUBROUTINE noise(       num_vis,
     *                        vis,
     *                        mean, sigma,
     *                        action,
     *                        s               )

C
C     Applies noise to the given visibility buffer.
C     returns immediately if the mean and sigma are both 0.
C
C     Given:
C         Number of spacings.
              integer*4       num_vis
C         Visibility buffer.
              complex         vis( num_vis )
C         mean and sigma of noise distribution
              real            mean, sigma
C         action control  = 1 complex multiply vis by noise
C                         = 2 add noise to vis
              integer         action
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     uses NAGLIB routines in non-repeatable way.
C         because these seem to cause overflows(?) which are trapped
C         in POSTMORTEM and dealt with (slowly) I have turned off
C         exception handling within this sr.
C                                               PJW  4/3/92
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/global_constants.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         naglib function and variables
              real*8          G05DDF, dmean, dsigma
C         Calibration data
              complex         noise_list( max_vis )

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
C     return if nothing to do
      if ( mean.eq.0. .and. sigma.eq.0. ) return
      call io_setexc( .false. , s )
      call G05CCF
      if (s.ne.0) write(1,*) 'status after G05ccf',s
      dmean = mean
      dsigma = sigma
C      write(1,*)'dmean dsigma num_vis action'
C     *               ,dmean,dsigma,num_vis,action
C     ****************************************************************
C
C         Main Code
C         ---------
C
      s = 0
      do i = 1, num_vis
          noise_list(i) = cmplx( sngl( G05DDF( dmean, dsigma ) ),
     *                           sngl( G05DDF( dmean, dsigma ) ) )
      enddo

      if (s.ne.0) write(1,*) 'status after G05ddf', s
C     write(1,*)'vis noise',vis(1),noise_list(1)
      s = 0
      if ( action .eq. 1 ) then
          do 100, i = 1, num_vis
              vis( i ) = vis( i ) * noise_list( i )
  100     continue
      elseif( action .eq. 2 ) then
          do 200, i = 1, num_vis
              vis( i ) = vis( i ) + noise_list( i )
  200     continue
      end if
      call io_setexc( .true. , s )
      if (s.ne.0) write(1,*)'status after application of noise',s
      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine noise' )
          return
      end
