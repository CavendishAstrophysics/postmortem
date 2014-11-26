C***********************************************************************
C
C  /mrao/post/include/control_tab_v0.inc
C
C
C  CAMBRIDGE LOW FREQUENCY SYNTHESIS TELESCOPE (CLFST) CONTROL TABLES
C  ------------------------------------------------------------------
C
C  This file contains the definition of version 0 of the control tables.
C  This version is the one used by the CLFST during an observing run and
C  contains the parameters that are used to define and control the
C  telescope during an observing run.
C
C  The control tables can be used in two ways:
C  1. If they are to be used locally to a routine then pre-declare the
C     integer variable CT_ALL.
C  2. If they are to be used globally then pre-include the file
C     /mrao/post/include/control_tables.inc which declares the named
C     common area /CONTROL_TABLES/.
C
C  Any copying can be done using the I*4 array CTV0ALL, which is
C  declared in this file to a length of 1024 (or CTV0LEN) elements.
C
C  DJT, 30 June 92, Unix implementation
C
C***********************************************************************
C
       INTEGER        CTV0LEN
       PARAMETER    ( CTV0LEN = 512*4)

       INTEGER*4      CTV0ALL(CTV0LEN)
       EQUIVALENCE  ( CTV0ALL(1), CT_ALL )

       include '/mrao/post/include/clfst_constants.inc'

C  Section 1 : Telescope status information
C  ----------------------------------------
C
       INTEGER*2    CTV0S1(1680)
       EQUIVALENCE (CTV0S1(1),CTV0ALL(1))
C
C     Array containing the indexes for east and west aerials for
C     each spacing in the current configuration.  The entries are
C     ordered (but NOT indexed) according to spacing.
C
         INTEGER*2  ISPAE(2,MAX_SPAC)

C     Aerial status array - information on a given aerial is held as a
C     bit combination. The status bits are continually modified during
C     a run. The status bits are :-
C               Bit 15  Aerial in use
C               Bit 14  Aerial tracking
C               Bit 13  Aerial receiving
C               Bit 12  set if gross error in Dec
C               Bit 11  set if gross error in RA
C               Bit 10  set if Dec pointing noisy
C               Bit 9   set if RA pointing noisy
C
         INTEGER*2  IAESTAT(MAX_AES)

C     Hut status array - information on a given hut is held as a
C     bit combination. The status bits are continually modified during
C     a run. The status bits are :-
C               Bit 15  set if hut on-line and operational
C               Bit 14  set if hut receiving data
C               Bit 13  set if aerials tracking
C               Bit 12  set if hut temporarily off-line
C               Bit 11  set if hut arguing
C               Bit 10  set if local oscillator out of lock
C
         INTEGER*2  IHUTSTAT(MAX_HUTS)

C     Sample file version number. This is always equal to 0 for this
C     verion of the control tables definition.
C
         INTEGER*2    CTV0_VERS

C     Telescope Identifier. The telescopes currently recognised are
C     T151, T38 and VLBI (1,2 and 3).
C
         INTEGER*2    ITSCOPE
C
C     The user identifier of the most recent access is held in the
C     control tables.
C
         CHARACTER*16   CTV0_USER
C
         EQUIVALENCE (CTV0S1(1),   ISPAE)
         EQUIVALENCE (CTV0S1(1601),IAESTAT)
         EQUIVALENCE (CTV0S1(1661),IHUTSTAT)
         EQUIVALENCE (CTV0S1(1669),CTV0_VERS)
         EQUIVALENCE (CTV0S1(1670),ITSCOPE)
         EQUIVALENCE (CTV0S1(1671),CTV0_USER)


C  Section 2 : Telescope geometry information
C  ------------------------------------------
C
       INTEGER*2    CTV0S2(400)
       EQUIVALENCE (CTV0S2(1),CTV0ALL(841))
C
C     X, Y, and Z coordinates of each aerial in wavelengths. The
C     coordinate system is centred near the telescope centre and is
C     aligned so that:
C         Z   is in the direction of the North Celestial Pole
C         X   is in perpendicular to Z and in the plane defined
C             by the Z axis and the 5 km datum line.
C         Y   is perpendicular to both.
C
C     The 5 km datum line is define below by ALONG, ELEV and AZIM.
C
         REAL*4     X(MAX_AES), Y(MAX_AES), Z(MAX_AES)
C
C     Longitude of the telescope in arcseconds (NOT seconds of time).
C     This is not a geographically significant point for the 5 km datum
C     line. (ie. it is not tangent to the earth's surface at this point)
C     However, it is used to define the coordinate system that the
C     datum line is expressed in - the tangent to the earth's surface at
C     this point defines the zero of azimuth for the system.
C
C     The value is 10.46 sec corresponding to 2.615 minutes of time
C     East of Greenwich - the Nautical Almanac says MRAO is at +2.6'.
C
         REAL*4     ALONG
C
C     X, Y, Z coordinates of two points defining a reference line
C     approximately parallel and south of the telescope
C
         REAL*4     REFPT(6)
C
C     Latitude, elevation and azimuth of telescope datum line in
C     degrees, arcminutes and arcseconds.  These are used to convert
C     the aerial XYZ coordinates in wavelengths to chainage, northage
C     and height coordinates in metres;  and vice versa.  The latter
C     coordinates are recorded on the disc files  GEOMETRY:SYMB  for
C     151MHz and 38MHz, and are available for re-initialising the
C     values in the control tables.
C
C     The zero of azimuth is defined by ALONG (above) and is measured
C     positive clockwise when looking down on the North Pole.
C
         INTEGER*2  LAT(3), ELEV(3), AZIM(3)
C
C     Date (day,month,year) of last update of telescope geometry.
C
         INTEGER*2  IGDAT(3)
C
         EQUIVALENCE (CTV0S2(1),  X)
         EQUIVALENCE (CTV0S2(121),Y)
         EQUIVALENCE (CTV0S2(241),Z)
         EQUIVALENCE (CTV0S2(361),ALONG)
         EQUIVALENCE (CTV0S2(363),REFPT)
         EQUIVALENCE (CTV0S2(375),LAT)
         EQUIVALENCE (CTV0S2(378),ELEV)
         EQUIVALENCE (CTV0S2(381),AZIM)
         EQUIVALENCE (CTV0S2(384),IGDAT)


C  Section 3 : Telescope calibration information
C  ---------------------------------------------

       INTEGER*2    CTV0S3(256)
       EQUIVALENCE (CTV0S3(1),CTV0ALL(1041))
C
C     Collimation phases for each aerial in degrees.
C
         REAL*4       COLL(MAX_AES)

C     Amplitude scaling factors for each aerial.
C
         REAL*4       AMPF(MAX_AES)

C     Text definition for units symbol e.g. 'Jy'
C
         CHARACTER*8  AUNITS

C     Scales internal units (integer*2) to/from external units as
C     defined by AUNITS.
C
         REAL*4       AMPSCL

C     Cutoff amplitude during run in external units. To provide
C     maximum resolution in the internally stored data the following
C     relation holds AFAC=32767/(ACHOP*AMPSCL)
C
         REAL*4       ACHOP

C     Flag signifying application of AMPF factors
C
         INTEGER*4    IAMPF

C     Date (day,month,year) of last update to AMPF factors.
C
         INTEGER*2   IADAT(3)

C     Date (day,month,year) of last update to COLL factors.
C
         INTEGER*2   ICDAT(3)
C
         EQUIVALENCE (CTV0S3(1),  COLL)
         EQUIVALENCE (CTV0S3(121),AMPF)
         EQUIVALENCE (CTV0S3(241),AUNITS)
         EQUIVALENCE (CTV0S3(245),AMPSCL)
         EQUIVALENCE (CTV0S3(247),ACHOP)
         EQUIVALENCE (CTV0S3(249),IAMPF)
         EQUIVALENCE (CTV0S3(251),IADAT)
         EQUIVALENCE (CTV0S3(254),ICDAT)


C  Section 4 : Real time control parameters
C  ----------------------------------------

       INTEGER*2  CTV0S4(40)
       EQUIVALENCE (CTV0S4(1),CTV0ALL(1201))
C
C     Correlator cycle phase times in 10ths of a millisecond
C     i.e. integration time and read time
C
         INTEGER*2    ITAB(2)

C     The expected 'zero correlation' count from the 'one-up'
C     counters. This is generally used as a check for correlator
C     malfunctions.
C
         INTEGER*4    IZERO

C     Sampling time (integration and read) for a raw correlator
C     sample in 10ths of a second. (Normally 15).
C
         INTEGER*4    ISAMPS

C     Sidereal time for current sample in 10ths of a second. This
C     value as well as being stored with each sample is used at run
C     time to check for correlator timing errors.
C
         INTEGER*4   ISTS

C     Last time MICROPC was called in 10ths of a second.
C
         INTEGER*4    ITIMPC

C     Number of hut currently online.
C
         INTEGER*2    IONLINE

C     Phase switch cycle counter (0-3). The phase switches allow for
C     insertion of 0/4, 1/4, 2/4, 3/4 wavelengths.
C
         INTEGER*2    ICYC

C     Selected C.P.U. 1 - ND100, 2 - ND500 (obsolete, ICPU=2)
C
         INTEGER*2    ICPU

C     Phase switching enabled flag
C
         INTEGER*2    IPHSW

C     Selected oscillator (local or external) - the external oscillator
C     (100KHz sidereal) is normally selected for real-time correlator
C     control.
C
         INTEGER*2    IOSC

C     If set the aerials will automatically be stowed after a run.
C
         INTEGER*2    ISTOW

C     Run in progress flag, set after the first sample is acquired.
C
         INTEGER*2    ISTART

C     Run stopped flag, set by RT-program PURGE when run terminates
C     normally.
C
         INTEGER*2    ISTOP

C     GMT/BST flag, offset of local cpu clock from UT in hours.
C
         INTEGER*2    IUTIM
C
         EQUIVALENCE (CTV0S4(1), ITAB)
         EQUIVALENCE (CTV0S4(3), IZERO)
         EQUIVALENCE (CTV0S4(5), ISAMPS)
         EQUIVALENCE (CTV0S4(7), ISTS)
         EQUIVALENCE (CTV0S4(9), ITIMPC)
         EQUIVALENCE (CTV0S4(11),IONLINE)
         EQUIVALENCE (CTV0S4(12),ICYC)
         EQUIVALENCE (CTV0S4(13),ICPU)
         EQUIVALENCE (CTV0S4(14),IPHSW)
         EQUIVALENCE (CTV0S4(15),IOSC)
         EQUIVALENCE (CTV0S4(16),ISTOW)
         EQUIVALENCE (CTV0S4(17),ISTART)
         EQUIVALENCE (CTV0S4(18),ISTOP)
         EQUIVALENCE (CTV0S4(19),IUTIM)


C  Section 5 : Real time control information
C  -----------------------------------------

       INTEGER*2  CTV0S5(20)
       EQUIVALENCE (CTV0S5(1),CTV0ALL(1221))
C
C     DMA device number - 400B
C
         INTEGER*2    ADEV

C     Programmable clock device number - 470B
C
         INTEGER*2    CDEV

C     Monitor output unit (Fortran unit 1)
C
         INTEGER*2    MONDEV

C     Nord/Micro communication line (TTY39)
C
         INTEGER*2    MICRO

C     Baud rate for line TTY39
C
         INTEGER*2    MSPEED

C     Error device used flag
C
         INTEGER*2    IERDEV

C     Error device line number
C
         INTEGER*2   ERRDEV

C     Internal device 1 (device number 200B). The internal device lines
C     are used for communication between the correlator programs and
C     the rest of the system - i.e. MICROPC and AMPHI.
C
         INTEGER*2    INDEV1

C     Internal device 2 (device number 201B)
C
         INTEGER*2    INDEV2

C     Semaphore (Lock) for AMPHI/Correlator communication.
C
         INTEGER*2    ISEM1

C     Semaphore (Lock) for MICROPC/Correlator communication.
C
         INTEGER*2    ISEM2

C     Semaphore used by OBSSTAT
C
         INTEGER*2    ISEM3
C
         EQUIVALENCE (CTV0S5(1), ADEV)
         EQUIVALENCE (CTV0S5(2), CDEV)
         EQUIVALENCE (CTV0S5(3), MONDEV)
         EQUIVALENCE (CTV0S5(4), MICRO)
         EQUIVALENCE (CTV0S5(5), MSPEED)
         EQUIVALENCE (CTV0S5(6), IERDEV)
         EQUIVALENCE (CTV0S5(7), ERRDEV)
         EQUIVALENCE (CTV0S5(8), INDEV1)
         EQUIVALENCE (CTV0S5(9), INDEV2)
         EQUIVALENCE (CTV0S5(10),ISEM1)
         EQUIVALENCE (CTV0S5(11),ISEM2)
         EQUIVALENCE (CTV0S5(12),ISEM3)


C  Section 6 : Telescope and fundamental constants
C  -----------------------------------------------

       INTEGER*2   CTV0S6(40)
       EQUIVALENCE (CTV0S6(1),CTV0ALL(1241))

C     pi = 3.1415926535897932364, 2*pi
C
         REAL*8       PI, TWOPI

C     Radians per second of time, radians per degree
C
         REAL*8       RADPS, RADPD

C     Speed of light in vacuo (2.997925e+8 m/s)
C
         REAL*8       C

C     Observation Frequency (151.5 MHz)
C
         REAL*8       FROBS

C     Path compensation and digitisation frequency
C
         REAL*8       FROSC

C     Intermediate frequency - originally a PC unit fudge factor
C
         REAL*8       PCFAC

C     Nominal spacing unit in metres (5.98)
C
         REAL*8       USPAC

C     Nominal east-west spacing in wavelengths (calculated)
C
         REAL*8       SPFAC
C
         EQUIVALENCE (CTV0S6(1), PI)
         EQUIVALENCE (CTV0S6(5), TWOPI)
         EQUIVALENCE (CTV0S6(9), RADPS)
         EQUIVALENCE (CTV0S6(13),RADPD)
         EQUIVALENCE (CTV0S6(17),C)
         EQUIVALENCE (CTV0S6(21),FROBS)
         EQUIVALENCE (CTV0S6(25),FROSC)
         EQUIVALENCE (CTV0S6(29),PCFAC)
         EQUIVALENCE (CTV0S6(33),USPAC)
         EQUIVALENCE (CTV0S6(37),SPFAC)


C  Section 7 : Current observation information
C  -------------------------------------------

       INTEGER*2  CTV0S7(80)
       EQUIVALENCE (CTV0S7(1),CTV0ALL(1401))
C
C     Title for observation
C
         CHARACTER*16  TITLE

C     Map redtape title
C
         CHARACTER*12  SOURCE

C     RA, Dec at date for observation PC centre (radians)
C
         REAL*8       RAMC, DECMC

C     Date at midday of the observation expressed as a decimal year.
C
         REAL*8       DATOBS

C     RA, Dec of observation PC centre at reference epoch (radians)
C
         REAL*8       RAREF, DECREF

C     Reference epoch expressed as a decimal year (usually 1950.0).
C
         REAL*8       DATREF

C     Starting hour angle for run
C
         REAL*8       STHA

C     Total number of spacings used, active size of array ISPAE.
C
         INTEGER*4    NSP

C     INTSAM is the count of raw samples (cos and sin) which are
C     phase rotated to the map centre and integrated in real-time,
C     before dumping as a sample in the sample file.  The following
C     holds for samples in the file :
C
C        Integration time = 2*ISAMPS*INTSAM/10 seconds
C
         INTEGER*4    INTSAM

C     Total number of samples in the sample file
C
         INTEGER*4    NSAMP

C     Number of samples with rejected raw samples (??)
C
         INTEGER*4    NCHOP

C     Number of samples for which more than half of the raw samples
C     were rejected.
C
         INTEGER*4    NBAD

C     Date (day,month,year) at beginning and end of run
C
         INTEGER*2    IDAT1(3), IDAT2(3)

C     Local time (secs,mins,hours) at beginning and end of run
C
         INTEGER*2    ITIM1(3), ITIM2(3)

C     Sidereal time (secs,mins,hours) at beginning and end of run
C
         INTEGER*2    ISTIM1(3), ISTIM2(3)
C
         EQUIVALENCE (CTV0S7(1), TITLE)
         EQUIVALENCE (CTV0S7(9), SOURCE)
         EQUIVALENCE (CTV0S7(17),RAMC)
         EQUIVALENCE (CTV0S7(21),DECMC)
         EQUIVALENCE (CTV0S7(25),DATOBS)
         EQUIVALENCE (CTV0S7(29),RAREF)
         EQUIVALENCE (CTV0S7(33),DECREF)
         EQUIVALENCE (CTV0S7(37),DATREF)
         EQUIVALENCE (CTV0S7(41),STHA)
         EQUIVALENCE (CTV0S7(45),NSP)
         EQUIVALENCE (CTV0S7(47),INTSAM)
         EQUIVALENCE (CTV0S7(49),NSAMP)
         EQUIVALENCE (CTV0S7(51),NCHOP)
         EQUIVALENCE (CTV0S7(53),NBAD)
         EQUIVALENCE (CTV0S7(55),IDAT1)
         EQUIVALENCE (CTV0S7(58),IDAT2)
         EQUIVALENCE (CTV0S7(61),ITIM1)
         EQUIVALENCE (CTV0S7(64),ITIM2)
         EQUIVALENCE (CTV0S7(67),ISTIM1)
         EQUIVALENCE (CTV0S7(70),ISTIM2)


C  Section 8 : Aerial information
C  ------------------------------

       INTEGER*2  CTV0S8(256)
       EQUIVALENCE (CTV0S8(1),CTV0ALL(1441))

C     Offsets in each aerial pointing from RAAE & DECAE  due to slack
C     in mechanisms etc. These are permanent values as distinct from
C     offsets due to nodding of aerials.
C
         REAL*4       HOFFSET(MAX_AES), DOFFSET(MAX_AES)

C     RA, Dec pointing for aerials during run
C
         REAL*8       RAAE, DECAE

C     Aerial pointing flag.  Set for non-standard pointing (i.e.
C     aerials not pointing to the phase centre).
C
         INTEGER*2    ISETAE

C     Drive interval for aerials.  Aerial pointing is updated every
C     INTAE*INTPC seconds in real-time.
C
         INTEGER*2    INTAE
C
         EQUIVALENCE (CTV0S8(1),  HOFFSET)
         EQUIVALENCE (CTV0S8(121),DOFFSET)
         EQUIVALENCE (CTV0S8(241),RAAE)
         EQUIVALENCE (CTV0S8(245),DECAE)
         EQUIVALENCE (CTV0S8(249),ISETAE)
         EQUIVALENCE (CTV0S8(250),INTAE)


C  Section 9 : Path compensator information
C  ----------------------------------------

       INTEGER*2  CTV0S9(130)
       EQUIVALENCE (CTV0S9(1),CTV0ALL(1601))
C
C     PC offset from mean PC pointing in PC units. Can be used for
C     PC nodding.
C
         INTEGER*2    POFFSET(MAX_AES)

C     Current PC values
C
         INTEGER*2    IOPC(MAX_AES)

C     Mean RA and Dec for PC beam (radians)
C
         REAL*8       RAPC, DECPC

C     PC pointing flag.  Set if the RA and Dec for PC beam are not
C     the same as the phase centre.
C
         INTEGER*2    ISETPC

C     Time interval for PC update
C
         INTEGER*2    INTPC
C
         EQUIVALENCE (CTV0S9(1),  POFFSET)
         EQUIVALENCE (CTV0S9(61), IOPC)
         EQUIVALENCE (CTV0S9(121),RAPC)
         EQUIVALENCE (CTV0S9(125),DECPC)
         EQUIVALENCE (CTV0S9(129),ISETPC)
         EQUIVALENCE (CTV0S9(130),INTPC)


C  Section 10 : Calibration source information
C  -------------------------------------------

       INTEGER*2  CTV0S10(100)
       EQUIVALENCE (CTV0S10(1),CTV0ALL(1681))
C
C     Calibration source names
C
         CHARACTER*12  CSOURCE(6)

C     RA/DEC positions for calibrators (radians)
C
         REAL*8       CAL(2,6)

C     Number of calibration spacings
C
         INTEGER*4    NSPCAL

C     Integration time for calibration samples (cf INTSAM)
C
         INTEGER*4    INTCAL

C     Used by mapper for calibration sample smoothing.
C
         INTEGER*4    LIMCAL

C     Total sample count in calibration file
C
         INTEGER*4    NSAMPC

C     Number of calibrators in file
C
         INTEGER*4    NCAL

C     Used to reference a single calibrator
C
         INTEGER*4    ICAL

C     Count of corrections in sample file
C
         INTEGER*4    IONSPH

C     Effective spacing for which correction is held in wavelengths
C
         REAL*4       FION
C
         EQUIVALENCE (CTV0S10(1), CSOURCE)
         EQUIVALENCE (CTV0S10(37),CAL)
         EQUIVALENCE (CTV0S10(85),NSPCAL)
         EQUIVALENCE (CTV0S10(87),INTCAL)
         EQUIVALENCE (CTV0S10(89),LIMCAL)
         EQUIVALENCE (CTV0S10(91),NSAMPC)
         EQUIVALENCE (CTV0S10(93),NCAL)
         EQUIVALENCE (CTV0S10(95),ICAL)
         EQUIVALENCE (CTV0S10(97),IONSPH)
         EQUIVALENCE (CTV0S10(99),FION)


C  Section 11 : Source removal information
C  ---------------------------------------

       INTEGER*2  CTV0S11(128)
       EQUIVALENCE (CTV0S11(1),CTV0ALL(1731))

C     Remove source name
C
         CHARACTER*12  RSOURCE(6)

C     RA/DEC positions for remove sources
C
         REAL*8       REM(2,6)

C     Sample count over which to average
C
         INTEGER*4    INTREM(6)

C     Ionospheric correction applied flag
C
         INTEGER*4    LIMREM(6)

C     Number of remove samples in file
C
         INTEGER*4    NSAMPR(6)

C     Number of remove sources
C
         INTEGER*4    NREM

C     References a single remove source
C
         INTEGER*4    IREM
C
         EQUIVALENCE (CTV0S11(1),  RSOURCE)
         EQUIVALENCE (CTV0S11(37), REM)
         EQUIVALENCE (CTV0S11(85), INTREM)
         EQUIVALENCE (CTV0S11(97), LIMREM)
         EQUIVALENCE (CTV0S11(109),NSAMPR)
         EQUIVALENCE (CTV0S11(121),NREM)
         EQUIVALENCE (CTV0S11(123),IREM)


C  Section 12 : Sample file length and packing information
C  -------------------------------------------------------

       INTEGER*2  CTV0S12(48)
       EQUIVALENCE (CTV0S12(1),CTV0ALL(1801))
C
C     Unit number for open sample file
C
         INTEGER*4    ISMFIL

C     Sample file type code (1 - Normal 2 - Calibration, 3 - Remove)
C
         INTEGER*4    ISMTYP

C     Length of control tables in pages
C
         INTEGER*4    ICTAB

C     Store required for a single sample (all spacings) cos or sin.
C     A record is all spacings, cos and sin for a given sample
C     and requires 2 pages (2*LRECB bytes)
C
         INTEGER*4    LRECB

C     Number of records per buffer (8 pages)
C
         INTEGER*4    NPACK

C     Next block to be read
C
         INTEGER*4    IBLK

C     Number of blocks per buffer
C
         INTEGER*4    NBLK

C     Maximum block number in file
C
         INTEGER*4    MBLK

C     When calibration and real time removes are are being performed
C     more than one type of sample is packed into the buffer. MCAL and
C     MREM provide pointers to the necessary samples
C
         INTEGER*4    MCAL, MREM(6)

C     Normalisation factor for Real/Integer sample conversion.
C
         REAL*4       AFAC
C
C     Length of each sample in words.
C     Start of redtape within each sample.
C     Length of redtape in each sample.
C     Start of visibility data in each sample.
C     Start of Monitor block in each sample.
C     Length of Monitor block in each sample.

         EQUIVALENCE (CTV0S12(1), ISMFIL)
         EQUIVALENCE (CTV0S12(3), ISMTYP)
         EQUIVALENCE (CTV0S12(5), ICTAB)
         EQUIVALENCE (CTV0S12(7), LRECB)
         EQUIVALENCE (CTV0S12(9), NPACK)
         EQUIVALENCE (CTV0S12(11),IBLK)
         EQUIVALENCE (CTV0S12(13),NBLK)
         EQUIVALENCE (CTV0S12(15),MBLK)
         EQUIVALENCE (CTV0S12(17),MCAL)
         EQUIVALENCE (CTV0S12(19),MREM)
         EQUIVALENCE (CTV0S12(31),AFAC)


C  Section 13 : Map information (now obsolete)
C  -------------------------------------------

       INTEGER*2  CTV0S13(32)
       EQUIVALENCE (CTV0S13(1),CTV0ALL(1825))
C
C     Real time mapping in progress
C
         INTEGER*4    ISTMAP

C     Map size for X axis
C
         INTEGER*4    MAPSZX

C     Map size for Y axis
C
         INTEGER*4    MAPSZY

C     Seconds of arc per grid point along X axis
C
         REAL*4       SECARC
C
         EQUIVALENCE (CTV0S13(1),ISTMAP)
         EQUIVALENCE (CTV0S13(3),MAPSZX)
         EQUIVALENCE (CTV0S13(5),MAPSZY)
         EQUIVALENCE (CTV0S13(7),SECARC)


C  Section 14 : Nodding, exercising and PC cycle information
C  ---------------------------------------------------------

       INTEGER*2  CTV0S14(64)
       EQUIVALENCE (CTV0S14(1),CTV0ALL(1841))
C
C     Aerial/PC nodding parameters
C
         INTEGER*2    ICYCLE(2,16)

C     Style of cycle
C
         INTEGER*2    KCYCLE

C     Number of phases in cycle
C
         INTEGER*2    NCYCLE

C     Maximum number of phases possible in cycle
C
         INTEGER*2    MCYCLE

C     Nodding flag
C
         INTEGER*2    ISETCYC

C     Period of cycle
C
         INTEGER*2    INTCYC
C
         EQUIVALENCE (CTV0S14(1),ICYCLE)
         EQUIVALENCE (CTV0S14(33),KCYCLE)
         EQUIVALENCE (CTV0S14(34),NCYCLE)
         EQUIVALENCE (CTV0S14(35),MCYCLE)
         EQUIVALENCE (CTV0S14(36),ISETCYC)
         EQUIVALENCE (CTV0S14(37),INTCYC)


C  Section 15 : Observing files information
C  ----------------------------------------

       INTEGER*2  CTV0S15(80)
       EQUIVALENCE (CTV0S15(1),CTV0ALL(1881))

C     Sample, calibration and zero file names
C
         CHARACTER*40  SFILE, CFILE, ZFILE

C     Length in characters of sample, calibration and zero file names
C
         INTEGER*4    LSF, LCF, LZF

C     Switch to suppress zero file corrections.
C
         INTEGER*4    IZF

C     Save flag for sample file
C
         INTEGER*4    ISAVED
C
         EQUIVALENCE (CTV0S15(1), SFILE)
         EQUIVALENCE (CTV0S15(21),CFILE)
         EQUIVALENCE (CTV0S15(41),ZFILE)
         EQUIVALENCE (CTV0S15(61),LSF)
         EQUIVALENCE (CTV0S15(63),LCF)
         EQUIVALENCE (CTV0S15(65),LZF)
         EQUIVALENCE (CTV0S15(67),IZF)
         EQUIVALENCE (CTV0S15(69),ISAVED)


C  Section 16 : Observer comment
C  -----------------------------

       INTEGER*2  CTV0S16(256)
       EQUIVALENCE (CTV0S16(1),CTV0ALL(1921))
C
C     Comment text for sample file
C
         CHARACTER*512  CTEXT
         EQUIVALENCE   (CTEXT,CTV0S16(1))


