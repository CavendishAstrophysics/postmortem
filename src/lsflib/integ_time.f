
C+integ_time

      subroutine integ_time(   num_vis,
     *                        vis,
     *                        base,
     *                        ra1, dec1,
     *                        ra2, dec2,
     *                        sid,
     *                        skew,
     *                        integration_time,
     *                        s           )

C     Applies integ_time smearing to a complex visibility buffer

C     Given:
C         Number of visibilities in the buffer.
              integer         num_vis
C         Visibility buffer
              complex         vis( num_vis )
C         Base line coordinates for visibility buffer
              real*8          base( 3, num_vis )
C         Initial RA and dec of visibility buffer (Observation centre)
              real*8          ra1, dec1
C         Current RA and dec of the visibility buffer.
              real*8          ra2, dec2
C         Sidereal time ( in 10ths of sid. secs since midnight )
              integer         sid
C         Angle relating baseline coordinate system to the sidereal time
              real*8          skew
C         integration time in seconds
              real            integration_time

C     Returned:
C         Status - must be 0 on entry, unchanged on exit.
              integer         s


C     Simulates integration time smearing on a given visibility buffer.
C       after subroutine bandwidth ( PJW  8/91 )
C
C     effect too small !  - error of 2pi in formula   fixed 3/92 by PJW.
C-
C     ***************************************************************
C
C     Constants
          include  '/mrao/include/constants.inc'
          include  '/mrao/post/include/lsflib_errors.inc'

C     Variables
C         Loop control variable
              integer     i
C         Pre calculated trigonometric values
              real*8      cdec1, cdec2, ha, chacd, shacd, sd
C         The total phase shift rate dphi/dt
              real*8      phi
C         The arithmetically convenient form
              real*8      integ_const
C         Visibility reduction factor
              real*8      integ_fact
C
C     ****************************************************************
C
C     Subroutine initialisation

      if ( s .ne. 0 ) return
      if ( integration_time .eq. 0 ) return

      integ_const = integration_time*const_pi/(24*3600)
C     this constant is still too small by 2pi
      integ_const = integ_const * 2. * const_pi

C     ****************************************************************
C
C         Main Code
C         ---------

      if ( ( ra1 .ne. ra2 ) .or. ( dec1 .ne. dec2 ) ) then
          ha    = (dfloat(sid)/10.0D+0) * const_st2r + skew

          cdec1 = dcos( dec1 )
          cdec2 = dcos( dec2 )
          shacd = - dsin(ha-ra2)*cdec2 + dsin(ha-ra1)*cdec1
          chacd = dcos(ha-ra2)*cdec2 - dcos(ha-ra1)*cdec1
          sd    = dsin(dec2)-dsin(dec1)

          do 1000, i = 1, num_vis
C            dphi/dt = (z_coord*sd + y_coord*shacd + x_coord*chacd)
              phi = base(3,i)*sd    +
     *              base(2,i)*shacd +
     *              base(1,i)*chacd

              if (dabs(phi) .le. 1.0D-6) then
                  integ_fact= 1.0
              else
                  phi = phi*integ_const
                  integ_fact = dsin(phi) / phi
              endif
              vis(i) = vis(i) * cmplx( integ_fact, 0.0 )
 1000     continue
      end if

      if (s .ne. 0) goto 9999
      return

C     **** Error handling **********************************************

 9999 continue
          call lsf_wrerr( s, 'in subroutine integ_time' )
          return
      end
