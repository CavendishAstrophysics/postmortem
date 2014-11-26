
* Constants or slowly-varying parameters for the 5-km telescope
* =============================================================

* for use as include file: needs 5km-constants:incl

* 14 Feb 92
* 6 July 92  DJT, alignment adjusted for Sun f77 compiler


* 5-km telescope
*
* Parameters (geometry, frequency, encoder origins)
* =================================================
*
* The 'constants' describing the telescope are stored in a file
* (Telescope)tel-parameters:data; this should be referred to via
* the character parameter PARAMF which is set up in 5km-devices:incl

* These values may be altered by the program SET-TEL-PARAMS
* which should prompt for various values as needed. Various
* derived quantities will be recalculated.
*
* The data will be read into the telescope constants segment
* when the START-TELESCOPE or NEW-TELESCOPE-PARAMETERS command
* is obeyed.
* The values are then available to all RT programs if that
* segment is linked in.
*

      integer*2 tpa(1024)


* All frequencies in MHz unless otherwise noted
* Change of convention (March 89) : now specify LO frequencies
* true observing frequencies are equivalently
*     freq+-[subb_freq(i)]+chan_freq(j)  or
*     LO1 +-      [LO2(i)]+chan_freq(j)
* + if LO low [LOhigh = .false.], - if LO high [LOhigh = .true.]

      real*8 freq                    ! nominal centre freq
      real*8 LO1                     ! 1st LO
      equivalence (freq, LO1)

      real*8 ADrate                  ! sampling rate
*      real*8 ADclock(10)             ! A[low], A[high], B[low], ...

      real*8 subb_freq(max_subb)     ! centre of bands rel to FREQ
      real*8 LO2(max_subb)           ! 2nd LOs
      equivalence(subb_freq(1), LO2(1))

      real*8 chan_freq(max_channel)  ! centre of channels
*                                    ! relative to LO1 +- LO2 (v. above)

* Telescope geometry information
*
* all angles in radians unless otherwise noted
*
* longitude (east), az and elev of datum line, pole Y, latitude
* azimuth and elevation of the datum line in the tangent plane
* cos, sin latitude


      real*8 Elong, azdatum, eldatum, azlong, Ypole, latitude
      real*8 az_tangent, el_tangent
      real*4 cos_lat, sin_lat
      real*4 ha_stow, dec_stow
      real*4 ha_zenith, dec_zenith

* L128 is nominal baseline (ca. 4.6 km), metres;
* XQ, YQ : L128 * cos[sin](eldatum) / 512 * c : i.e. ns/quarter-spacing
* X8, Y8, Z8 : 'system 2' coordinates, ns: positive to W, S, NCP
*              including  xyz corrections; X4, Y4, Z4 are R*4 versions

      real*8   L128, XQ, ZQ, X8(8), Y8(8), Z8(8)
      real*4   X4(8), Y4(8), Z4(8)

      real*4   delay(8)      ! nom delays, ns

      logical*2 LOhigh       ! .F. if low, .T. if high
      integer*2 gap          ! min time between obs/min
      integer*2 config       ! 1 - 8 for standard config
      integer*2 aepos(4)     ! quarter-spacing aerial positions
      integer*2 opno(4)      ! sequence no of ob. positions in aepos
      integer*2 control(4)   ! control signals for ae 1 2 3 4
      integer*2 aestatus(8)  ! 1 track, 2 stow, 3 offset, 4 zen, 6 point
      integer*2 hardware_int ! ms
      integer*2 micro_int    ! hardware cycles before reading micros
      integer*2 micro_cycle_s     ! = (hardware_int/1000)*micro_int
      logical*2 data_compression  ! .TRUE. if micros compress data
      logical*2 corr_phase_sw     ! .TRUE. if switching applied
*                                 !        at phase rotators
      integer*2 Walsh_length      ! 8 or 16, for corr_phase_sw
      integer*2 ha_5point(0:4)    ! tables of pointing offsets
      integer*2 dec_5point(0:4)   ! multiplying factors (now 0,+-1)

      real*4    ha_7point(0:6)    ! R*4 multiplying factors
      real*4    dec_7point(0:6)   ! (now set up for hexagon + centre)
      integer*2 max_point_err     ! arcsec, reject if either ae worse

* Aerial position information, all dimensions are in 0.1mm
*       xyzae(3,8)  x,y,z/0.1mm for ae 1, 2,... 8
*                   [i.e. x(1),y(1),z(1),x(2),y(2),...]
*       xyzop(3,36) x,y,z/0.1mm for OP 1, 1.75, 2, ...
*       xyz(3,8)    x,y,z/0.1mm for ae 1 - 8
*                  = xyzae+xyzop for 1 - 4, xyzae for 5 - 8

      integer*2 xyzae(3,8), xyzop(3,36), xyz(3,8)

      integer*2 HOFS(8), DOFS(8)  ! ha and dec encoder offsets
      integer*2 aepoint(10, 8)    ! pointing corrs, as C0 - C7, H3, D3

* limits:   stored as R*4 radians
* N_limit, S_limit    absolute max/min;
* ha_limit            max |ha| (i.e. approx 90.5 deg)
* Sc_limit            min dec at +-ha_limit, with slope 2/3
*                       down to S_limit at less extreme ha

      real*4  N_limit, S_limit, ha_limit, Sc_limit

* Atmospheric parameters: all R*4

      real*4  Natmos              ! N-1 [typically 320E-6]
      real*4  Hatmos              ! scale height/m, ca 6000
      real*4  Rearth              ! 6 371 000 m

*     real*4  Clockerr            ! seconds, + if fast

* nominal system temperatures, Tamb (R*4) for rain correction
* nominal voltage from rain-detector at zenith

      real*4    Tsys(8)
      real*4    Tamb, Vrain0
      integer*2 rain_corr_on      ! 1 bit per aerial
*                                 ! bit (N-1) is 1 if correction
*                                 ! for aerial N is to be used
* also need table of data for elevation-dep of Tsys
*
      integer*2 version           ! 2 at present
      integer*2 update(3)         ! d, m, y of last update

* equivalence statements to the tpa array
* ---------------------------------------

      equivalence (tpa(1),          freq)
      equivalence (tpa(5),          ADrate)

      integer      Pp1, Pp2
      parameter   (Pp1 = 9   + 4*max_subb)        ! 29
      parameter   (Pp2 = Pp1 + 4*max_channel)     ! 61

      equivalence (tpa(9),          subb_freq)
      equivalence (tpa(Pp1),        chan_freq)
      equivalence (tpa(Pp2),        Elong)
      equivalence (tpa(Pp2+4),      azdatum)
      equivalence (tpa(Pp2+8),      eldatum)
      equivalence (tpa(Pp2+12),     azlong)
      equivalence (tpa(Pp2+16),     latitude)
      equivalence (tpa(Pp2+20),     Ypole)
      equivalence (tpa(Pp2+24),     L128)
      equivalence (tpa(Pp2+28),     XQ)
      equivalence (tpa(Pp2+32),     ZQ)
      equivalence (tpa(Pp2+36),     X8)
      equivalence (tpa(Pp2+68),     Y8)
      equivalence (tpa(Pp2+100),    Z8)
      equivalence (tpa(Pp2+132),    X4)
      equivalence (tpa(Pp2+148),    Y4)
      equivalence (tpa(Pp2+164),    Z4)
      equivalence (tpa(Pp2+180),    delay)

      integer      Pp3
      parameter   (Pp3=Pp2+196)                   ! 257

      equivalence (tpa(Pp3),        LOhigh)
      equivalence (tpa(Pp3+1),      gap)
      equivalence (tpa(Pp3+2),      config)
      equivalence (tpa(Pp3+3),      aepos)
      equivalence (tpa(Pp3+7),      opno)
      equivalence (tpa(Pp3+11),     control)
      equivalence (tpa(Pp3+15),     aestatus)
      equivalence (tpa(Pp3+23),     xyzae)
      equivalence (tpa(Pp3+47),     xyzop)
      equivalence (tpa(Pp3+155),    xyz)
      equivalence (tpa(Pp3+179),    HOFS)
      equivalence (tpa(Pp3+187),    DOFS)
      equivalence (tpa(Pp3+195),    aepoint)

      integer      Pp4
      parameter   (Pp4=Pp3+276)                    ! 533

      equivalence (tpa(Pp4),        Natmos)
      equivalence (tpa(Pp4+2),      Hatmos)
      equivalence (tpa(Pp4+4),      Rearth)
      equivalence (tpa(Pp4+6),      version)
      equivalence (tpa(Pp4+7),      update)

*
      integer Pp5
      parameter (Pp5 = Pp4+11)                     ! 544

*      equivalence (tpa(Pp5),         ... free)
      equivalence (tpa(Pp5+40),     data_compression)
      equivalence (tpa(Pp5+41),     hardware_int)
      equivalence (tpa(Pp5+42),     micro_int)
      equivalence (tpa(Pp5+43),     Tsys)
      equivalence (tpa(Pp5+59),     Tamb)
      equivalence (tpa(Pp5+61),     Vrain0)
      equivalence (tpa(Pp5+63),     rain_corr_on)

      equivalence (tpa(Pp5+65),     cos_lat)
      equivalence (tpa(Pp5+67),     sin_lat)
      equivalence (tpa(Pp5+69),     az_tangent)
      equivalence (tpa(Pp5+73),     el_tangent)
      equivalence (tpa(Pp5+77),     micro_cycle_s)
      equivalence (tpa(Pp5+78),     max_point_err)
      equivalence (tpa(Pp5+79),     ha_stow)
      equivalence (tpa(Pp5+81),     dec_stow)
      equivalence (tpa(Pp5+83),     ha_zenith)
      equivalence (tpa(Pp5+85),     dec_zenith)

      equivalence (tpa(Pp5+87),     N_limit)
      equivalence (tpa(Pp5+89),     S_limit)
      equivalence (tpa(Pp5+91),     ha_limit)
      equivalence (tpa(Pp5+93),     Sc_limit)

      equivalence (tpa(Pp5+95),     ha_5point)
      equivalence (tpa(Pp5+100),     dec_5point)
      equivalence (tpa(Pp5+105),    ha_7point)
      equivalence (tpa(Pp5+119),    dec_7point)
      equivalence (tpa(Pp5+133),    corr_phase_sw)
      equivalence (tpa(Pp5+134),    Walsh_length)
*     equivalence (tpa(Pp5+135),     ... next)

      integer    ppend
      parameter (ppend = Pp5+135)

* +++ end of include file +++

