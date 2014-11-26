C
C
C     *****************************************************************
C

C+cal_sel_bandwid
C
      SUBROUTINE cal_sel_bandwid( psf_name, s )

C     Asks the user to select the bandwidth for the calibration.
C
C     Given:
C         Current physical sample file name
              character*64        psf_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'

C
C     Variables, equivalences and commons
C         Loop counter
              integer             i
C         New value of telescope bandwidth
              real                new_bandwidth
C         Band-pass type
              character*14        reply
C         Valid band-pass types
              character*14        band_types(2)
              data  band_types / 'box-car', 'gaussian' /

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C     Main Code
C     ---------
C

C      new_bandwidth = cal_bandwidth/1.0E+6
      new_bandwidth = cal_bandwidth

200   call io_getr( 'FWHM bandwidth in MHz : ', '*', new_bandwidth, s )
      if( new_bandwidth .gt. 1000. ) goto 200
      call io_getopt( 'Telescope bandpass type (?=list) : ',
     *              band_types(cal_band_type),
     *              band_types, 2,
     *              reply, s      )

      if (s.eq.0) then
          do 100, i = 1, 2
              if (reply .eq. band_types(i)) cal_band_type = i
  100     continue

C          cal_bandwidth = abs(new_bandwidth)*1.0E+6
          cal_bandwidth = new_bandwidth
      end if

      if ( s .ne. 0 ) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call cal_wrerr( s, 'in subroutine cal_sel_bandwidth ' )
          end if
          return
      end
