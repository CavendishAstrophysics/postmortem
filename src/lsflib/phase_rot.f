
C     ******************************************************************
C
C+phase_rot

      subroutine phase_rot(   num_vis,
     *                        vis,
     *                        base,
     *                        ra1, dec1,
     *                        ra2, dec2,
     *                        sid,
     *                        skew,
     *                        ion,
     *                        s           )

C     Phase rotates a complex visibility buffer

C     Given:
C         Number of visibilities in the buffer.
              integer         num_vis
C         Visibility buffer
              complex         vis( num_vis )
C         Base line coordinates for visibility buffer
              real*8          base( 3, num_vis )
C         Current RA and dec of visibility buffer
              real*8          ra1, dec1
C         RA and dec for buffer to be rotated to
              real*8          ra2, dec2
C         Sidereal time ( in 10ths of sid. secs since midnight )
              integer         sid
C         Angle relating the coordinate system to the sidereal time.
              real*8          skew
C         Ionospheric correction to be applied - in radians/wavelength
              real            ion

C     Returned:
C         Status - must be 0 on entry, unchanged on exit.
              integer         s

C     Phase rotates the complex visibility buffer from RA1 dec1 to
C     RA2, dec2 usinge the geometry for each visibility set up in
C     the array base. The X ordinate of base must in the direction
C     the ionospheric correction must be done. See comment in the
C     routine calc_baseline for more description of this system.
C
C-
C     ***************************************************************
C
C     Constants
          include  '/mrao/include/constants.inc'

C     Variables
C         Loop control variable
              integer     i
C         Ionospheric correction and phase indicator flags
              logical     shift_flg, ion_flg
C         Pre calculated trigonometric values
              real*8      cdec1, cdec2, ha, chacd, shacd, sd
C             real*8      sha2_cotd2
C         The amount the x, y and z coordinates need to be multiplied by
C         to get the correct overall phase
              real*8      x_factor, y_factor, z_factor
C         The total phase shift
              real*8      phi

C     Code for using tabulated cos's and sines.
C         The cosine and sine of this shift
C             real        cosine, sine
c         include '/mrao/post/include/cossin_variables.inc'

C     ****************************************************************
C
C     Subroutine initialisation

      if ( s .ne. 0 ) return

      shift_flg = ( ( ra1 .ne. ra2 ) .or. ( dec1 .ne. dec2 ) )
      ion_flg   = ( ion .ne. 0.0 )

      if ( .not. ( shift_flg .or. ion_flg ) ) return

C     ****************************************************************
C
C         Main Code
C         ---------

      ha    = (dfloat(sid)/10.0D+0) * const_st2r + skew

      if ( ion_flg ) then
C         phi = ( x_coord - z_coord*sha2_cotd2 ) * ion
          x_factor = dfloat(ion)
          y_factor = 0.0D+0
          z_factor = -dfloat(ion) * dsin(ha-ra2) * dcos(dec2)/dsin(dec2)
      else
          x_factor = 0.0D+0
          y_factor = 0.0D+0
          z_factor = 0.0D+0
      end if

      if (shift_flg) then
          cdec1 = dcos( dec1 )
          cdec2 = dcos( dec2 )
          chacd = dcos(ha-ra2)*cdec2 - dcos(ha-ra1)*cdec1
          shacd = dsin(ha-ra2)*cdec2 - dsin(ha-ra1)*cdec1
          sd    = dsin(dec2)-dsin(dec1)

C         phi = phi + 2pi*(z_coord*sd + y_coord*chacd + x_coord*shacd)
          x_factor = x_factor + const_2pi*shacd
          y_factor = y_factor + const_2pi*chacd
          z_factor = z_factor + const_2pi*sd
      end if

C     Comments on speed:
C
C     During a full read of a sample file, with a source removal, the
C     LSF routines typically spend 60% of their time in the following
C     loop - proving how expensive sines and cosines are. However, there
C     is no way round the problem that I can see. I have tried the
C     following with the following problems.
C
C     single precision  - not accurate enough since at long baselines
C                         with big phase shifts the total phase is
C                         much greater than 2 pi, so the significant
C                         digits for sine and cosine are lost. This
C                         could be avoided by taking the angle mod
C                         two pi, but my tests indicate that dmod is
C                         actually slower than dsin. This also rules
C                         out all the microcode vector routines -
C                         since we don't have the double precision
C                         routines yet. (Sept 1987)
C     cexp( i*phi )       complex single precision exponentials are
C                         even slower than two dcos's - double
C                         precision are slower still.
C     sin=sqrt(1-cos**2)  Sqrt is about twice the speed of Cosine but
C                         you still need to work out which quadrant
C                         you're in, which requires a dmod, so it
C                         isn't worth it - also there are big roundoff
C                         errors when cos gets close to one.
C     Tabulation          I haven't tried this, it requires one dmod but
C                         we only have to get it right to a degree or so
C
C         At the moment, I think we're stuck with it.
C                                 Nick Rees   29 September 1987.

      do 1000, i = 1, num_vis
          phi = z_factor*base(3,i)+y_factor*base(2,i)+x_factor*base(1,i)
          phi = dmod(phi, const_2pi)

          vis(i) = vis(i) * cmplx( real(dcos(phi)), real(-dsin(phi)) )

C         Code for using tabulated cos's and sines.
C         call cossin( phi, cosine, sine )
c         include '/mrao/post/include/cossin_code.inc'
C         vis(i) = vis(i) * cmplx( cosine, -sine )
 1000 continue

      if (s .ne. 0) goto 9999
      return

C     **** Error handling **********************************************

 9999 continue
          call lsf_wrerr( s, 'in subroutine PHASE_ROT' )
          return
      end
