*=======================================================================
*
*  Track-Q-item
*  ============
*
* data to be stored as a q item
* and copied to TCA when observation is initiated.
*
* This file is included in the 5km Control Tables as the definition of
* the observation present in the sample file.


* last modified  7 Nov 94  ...  GP (comments added 13 Jan 99)

* NB: alignment of some items adjusted for Sun compiler.

*=====================================================================


      integer      max_Qitem, Qwords, Qbytes
      parameter   (max_Qitem = 511)
      parameter   (Qwords = max_Qitem+1)
      parameter   (Qbytes = 2*Qwords)

      integer*2    Qitem(0:max_Qitem)


      character*24 source          ! source name
      character*80 title           ! title of observation
      character*16 observer        ! name

* iepoch defines the reference epoch for all pointing centres

      integer*2    iepoch          ! 0, 1 for B1950, J2000
      real*8       RAref,  DECref  ! RA, DEC at reference date
      real*8       RAdate, DECdate ! RA, DEC at observation date

      real*8      MJDstart



* ST held as integer minutes (modulo 24h)
* observation is ST1 to ST2; a 12-h obs would start at ST0

      integer*2   ST0, ST1, ST2    ! Sidereal times as above
      integer*2   planet           ! 0 unless solar-system
      integer*2   Ncentre          ! no of pointing centres
*                                  !   1 to max_centre
      integer*2   centre           ! current centre (RT system)
*                                  ! centre for this sample file
      integer*2   integration      ! seconds per sample
      integer*2   offset  ! 0            : non-offset obs
*                         ! 1 = o_5point : pointing-offset on-N-S-W-E
*                         ! 2 = o_raster : raster
*                         ! 3 = o_7point : mosaic
*                         ! 4 = o_const  : constant [offset_W, offset_N]
*                         ! 5 = o_scan   : scan in HA, step in dec
*                         ! 6 = o_hex    : hex raster
*                         ! 7 = o_m5point: multiple-centre 5-point
      integer*2   offset_angle            ! arcmin on the sky
      real*4      offset_arcmin           ! arcmin on the sky
      integer*2   offset_aerial(max_aes)  ! 0 = not offset
*                                         ! 1 = offset applies
      integer*2   offset_time             ! samples per point or scan
      real*4      offset_W, offset_N      ! arcmin for type 4
      integer*2   offset2Nx, offset2Ny    ! max |offset2x, y|  type 2
      integer*2   offset_zero             ! samples at centre after
*                                         !      each row of raster
      integer*2   offset_row              ! first row of raster
      real*4      o_scan_HA               ! initial dH for each row
*                                         !  if < 0, scan in incr HA

      integer*2   micro_data      ! 0 : do not save raw data
*                                 ! 1 - 5 : save data from micro A - E

* items used to define the correlator configuration and sample file
* configuration

       integer*4  Nsp              ! number of spacings (usually 28)
       integer*4  Naes             ! number of aerials
       integer*4  Nsamp            ! total number of samples
       integer*4  Nsubb            ! number of sub-bands
       integer*4  Nchannel         ! number of channels

      integer*2   aerial_list(max_aes)    ! up to 8 aerial numbers
      integer*2   subband_list(max_subb)  ! up to 5 numbers
*                                         ! 1 for subband A, etc
*                                         ! both lists filled with 0
      real*4      clip_level              ! nominal Jy, max signal

* Definition of the feed rotation cycle (FRC):
* Each sample contains data for one polarization.
* A complete cycle consists of Npolns samples defined by ipoln_codes

      integer*2   feed_rotation_code
*                                  ! code for particular FRC
      integer*2   Npolns           ! number of polns. in cycle
      integer*2   ipoln_codes(10)  ! list of poln. codes


* items in arrays for multiple pointing centres

      character*24  source_list(max_centre)! source names
      character*40  file_list(max_centre)  ! sample file(s)
      integer*2     Tcentre(max_centre)    ! samples on this centre
      real*8        RAref_list(max_centre),
     *              DECref_list(max_centre)! RA/DECs at reference date

* ----------------------------------------------------------

      equivalence (Qitem( 0), source)
      equivalence (Qitem(12), title)
      equivalence (Qitem(52), observer)

      integer     Qp0
      parameter  (Qp0 = 64)

* R*8
      equivalence (Qitem(Qp0+0 ), RAref)
      equivalence (Qitem(Qp0+4 ), DECref)
      equivalence (Qitem(Qp0+8 ), RAdate)
      equivalence (Qitem(Qp0+12), DECdate)
      equivalence (Qitem(Qp0+16), MJDstart)

* I*2
      equivalence (Qitem(Qp0+20), iepoch)
      equivalence (Qitem(Qp0+21), ST0)
      equivalence (Qitem(Qp0+22), ST1)
      equivalence (Qitem(Qp0+23), ST2)
      equivalence (Qitem(Qp0+24), planet)
      equivalence (Qitem(Qp0+25), Ncentre)
      equivalence (Qitem(Qp0+26), micro_data)
      equivalence (Qitem(Qp0+27), centre)
      equivalence (Qitem(Qp0+28), integration)
      equivalence (Qitem(Qp0+29), offset)
      equivalence (Qitem(Qp0+30), offset_angle)
      equivalence (Qitem(Qp0+31), offset_time)
      equivalence (Qitem(Qp0+32), offset_aerial)

* I*4
      equivalence (Qitem(Qp0+40), Nsp)
      equivalence (Qitem(Qp0+42), Nsamp)
      equivalence (Qitem(Qp0+44), Naes)
      equivalence (Qitem(Qp0+46), Nsubb)
      equivalence (Qitem(Qp0+48), Nchannel)
* I*2
      equivalence (Qitem(Qp0+50), aerial_list)
      equivalence (Qitem(Qp0+58), subband_list)
      equivalence (Qitem(Qp0+63), feed_rotation_code)
      equivalence (Qitem(Qp0+64), Npolns)
      equivalence (Qitem(Qp0+65), ipoln_codes(1))

* following variables realigned to satisfy Sun Fortran compiler *******

* R*4
      equivalence (Qitem(Qp0+76), offset_W)
      equivalence (Qitem(Qp0+78), offset_N)
      equivalence (Qitem(Qp0+80), clip_level)

* I*2
      equivalence (Qitem(Qp0+82), offset_zero)
      equivalence (Qitem(Qp0+83), offset2Nx)
      equivalence (Qitem(Qp0+84), offset2Ny)
      equivalence (Qitem(Qp0+85), offset_row)

* R*4
      equivalence (Qitem(Qp0+86), offset_arcmin)
      equivalence (Qitem(Qp0+88), o_scan_HA)

* next free item Qitem(Qp0+90) = Qitem(154) to Qitem(159)

      integer      Qp1
      parameter   (Qp1 = 160)

      equivalence (Qitem(Qp1              ), Tcentre)         ! i*2
      equivalence (Qitem(Qp1+   max_centre), RAref_list)      ! r*8
      equivalence (Qitem(Qp1+ 5*max_centre), DECref_list)     ! r*8
      equivalence (Qitem(Qp1+ 9*max_centre), file_list)       ! c*40
      equivalence (Qitem(Qp1+29*max_centre), source_list)     ! c*24

* next free item: Qitem(488) if max_centre = 8

*  +++ end of track Q item +++

