
C     *****************************************************************
C
C+bandwidth

      subroutine bandwidth(   num_vis,
     *                        vis,
     *                        base,
     *                        ra1, dec1,
     *                        ra2, dec2,
     *                        sid,
     *                        skew,
     *                        band_type,
     *                        band_width,
     *                        s           )

C     Applies bandwidth smearing to a complex visibility buffer

C     Given:
C         Number of visibilities in the buffer.
              integer         num_vis
C         Visibility buffer
              complex         vis( num_vis )
C         Base line coordinates for visibility buffer
              real*8          base( 3, num_vis )
C         Initial RA and dec of visibility buffer (path comp. centre)
              real*8          ra1, dec1
C         Current RA and dec of the visibility buffer.
              real*8          ra2, dec2
C         Sidereal time ( in 10ths of sid. secs since midnight )
              integer         sid
C         Angle relating baseline coordinate system to the sidereal time
              real*8          skew
C         Bandwidth type (0=none, 1=boxcar, 2=gaussian)
              integer         band_type
C         Fractional bandwidth of telescope (FWHM/freq)
              real            band_width

C     Returned:
C         Status - must be 0 on entry, unchanged on exit.
              integer         s


C     Simulates bandwidth smearing on a given visibility buffer. The
C     bandpass is supposed to be a perfect box-car, so the bandpass
C     transform is a sinc function.
C
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
C         The total phase shift
              real*8      phi
C         The bandwidth's transform for this baseline and phase-shift.
              real        band_transform
C         The arithmetically convenient form for expressing bandwidth
              real*8      band_fact

C         The cosine and sine of phi
C             real        cosine, sine
c         include '/mrao/post/include/cossin_variables.inc'

C     ****************************************************************
C
C     Subroutine initialisation

      if ( s .ne. 0 ) return
      if ( band_width .eq. 0 .or. band_type .eq. 0) return

      if (band_type .eq. 1) then
          band_fact = band_width*const_pi
      else if (band_type .eq.2) then
          band_fact = band_width*const_piby2
          band_fact = band_fact*band_fact/log(2.0)
      else
          s = ILL_BANDTYPE
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------

      if ( ( ra1 .ne. ra2 ) .or. ( dec1 .ne. dec2 ) ) then
          ha    = (dfloat(sid)/10.0D+0) * const_st2r + skew

          cdec1 = dcos( dec1 )
          cdec2 = dcos( dec2 )
          chacd = dcos(ha-ra2)*cdec2 - dcos(ha-ra1)*cdec1
          shacd = dsin(ha-ra2)*cdec2 - dsin(ha-ra1)*cdec1
          sd    = dsin(dec2)-dsin(dec1)

          do 1000, i = 1, num_vis
C             phi = (z_coord*sd + y_coord*chacd + x_coord*shacd)
              phi = base(3,i)*sd    +
     *              base(2,i)*chacd +
     *              base(1,i)*shacd

              if (dabs(phi) .le. 1.0D-6) then
                  band_transform = 1.0
              else if (band_type .eq. 1) then
C                 Boxcar bandwidth - transform is sin(x)/x function.
                  phi = phi * band_fact
                  band_transform = dsin(phi) / phi

C                 Code for using tabulated cos's and sines.
C                 if (dabs(phi) .le. 1.0D-2) then
C                     band_transform = dsin(phi) / phi
C                 else
C                     call cossin( phi, cosine, sine )
c                     include '/mrao/post/include/cossin_code.inc'
C                     band_transform = sine / phi
C                 end if
              else
C                 Gaussian bandwidth
                  band_transform = exp(-real(band_fact*phi*phi) )
              end if

              vis(i) = vis(i) * cmplx( band_transform, 0.0 )
 1000     continue
      end if

      if (s .ne. 0) goto 9999
      return

C     **** Error handling **********************************************

 9999 continue
          call lsf_wrerr( s, 'in subroutine BANDWIDTH' )
          return
      end
