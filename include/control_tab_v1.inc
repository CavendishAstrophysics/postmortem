C***********************************************************************
C
C  /mrao/post/include/control_tab_v1.inc
C
C
C  CAMBRIDGE LOW FREQUENCY SYNTHESIS TELESCOPE (CLFST) CONTROL TABLES
C  ------------------------------------------------------------------
C
C  This file contains the definition of version 1 of the control tables.
C  This version is the one used by the POSTMORTEM system to define
C  remove files and calibration files for an observation.
C
C  The control tables can be used in two ways:
C  1. If they are to be used locally to a routine then pre-declare the
C     integer variable CT_ALL.
C  2. If they are to be used globally then pre-include the file
C     /mrao/post/include/control_tables.inc which declares the named
C     common area /CONTROL_TABLES/.
C
C  Any copying can be done using the I*4 array CTV1ALL, which is
C  declared in this file to a length of 1024 (or CTV1LEN) elements.
C
C***********************************************************************
C
       INTEGER        CTV1LEN
       PARAMETER    ( CTV1LEN = 512*6 )

       INTEGER*4      CTV1ALL(CTV1LEN)
       EQUIVALENCE  ( CTV1ALL(1), CT_ALL )

       include '/mrao/post/include/clfst_constants.inc'

C  Section 1 : Telescope status information
C  ----------------------------------------
C
       INTEGER*2    CTV1S1(1680)
       EQUIVALENCE (CTV1S1(1),CTV1ALL(1))
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

C     Sample file version number. Always equal to 1 for version 1
C     control tables (this version).
C
         INTEGER*2    CTV1_VERS

C     Telescope Identifier. The telescopes currently recognised are
C     T151, T38 and VLBI (1,2 and 3).
C
         INTEGER*2    ITSCOPE
C
C     User Identifier.  The user identifier of most recent access is
C     held in the control tables.
C
         CHARACTER*16 CTV1_USER
C
         EQUIVALENCE (CTV1S1(1),   ISPAE)
         EQUIVALENCE (CTV1S1(1601),IAESTAT)
         EQUIVALENCE (CTV1S1(1661),IHUTSTAT)
         EQUIVALENCE (CTV1S1(1669),CTV1_VERS)
         EQUIVALENCE (CTV1S1(1670),ITSCOPE)
         EQUIVALENCE (CTV1S1(1671),CTV1_USER)


C  Section 2 : Telescope geometry information
C  ------------------------------------------
C
       INTEGER*2    CTV1S2(400)
       EQUIVALENCE (CTV1S2(1),CTV1ALL(841))
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
         EQUIVALENCE (CTV1S2(1),  X)
         EQUIVALENCE (CTV1S2(121),Y)
         EQUIVALENCE (CTV1S2(241),Z)
         EQUIVALENCE (CTV1S2(361),ALONG)
         EQUIVALENCE (CTV1S2(363),REFPT)
         EQUIVALENCE (CTV1S2(375),LAT)
         EQUIVALENCE (CTV1S2(378),ELEV)
         EQUIVALENCE (CTV1S2(381),AZIM)
         EQUIVALENCE (CTV1S2(384),IGDAT)


C  Section 3 : Telescope calibration information
C  ---------------------------------------------

       INTEGER*2    CTV1S3(256)
       EQUIVALENCE (CTV1S3(1),CTV1ALL(1041))
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
         EQUIVALENCE (CTV1S3(1),  COLL)
         EQUIVALENCE (CTV1S3(121),AMPF)
         EQUIVALENCE (CTV1S3(241),AUNITS)
         EQUIVALENCE (CTV1S3(245),AMPSCL)
         EQUIVALENCE (CTV1S3(247),ACHOP)
         EQUIVALENCE (CTV1S3(249),IAMPF)
         EQUIVALENCE (CTV1S3(251),IADAT)
         EQUIVALENCE (CTV1S3(254),ICDAT)


C  Section 4 : Real time control parameters
C  ----------------------------------------

       INTEGER*2  CTV1S4(40)
       EQUIVALENCE (CTV1S4(1),CTV1ALL(1201))
C
C     Correlator cycle phase times in 10ths of a second
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
         EQUIVALENCE (CTV1S4(1), ITAB)
         EQUIVALENCE (CTV1S4(3), IZERO)
         EQUIVALENCE (CTV1S4(5), ISAMPS)
         EQUIVALENCE (CTV1S4(7), ISTS)
         EQUIVALENCE (CTV1S4(9), ITIMPC)
         EQUIVALENCE (CTV1S4(11),IONLINE)
         EQUIVALENCE (CTV1S4(12),ICYC)
         EQUIVALENCE (CTV1S4(13),ICPU)
         EQUIVALENCE (CTV1S4(14),IPHSW)
         EQUIVALENCE (CTV1S4(15),IOSC)
         EQUIVALENCE (CTV1S4(16),ISTOW)
         EQUIVALENCE (CTV1S4(17),ISTART)
         EQUIVALENCE (CTV1S4(18),ISTOP)
         EQUIVALENCE (CTV1S4(19),IUTIM)


C  Section 5 : Real time control information
C  -----------------------------------------

       INTEGER*2  CTV1S5(20)
       EQUIVALENCE (CTV1S5(1),CTV1ALL(1221))
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
         EQUIVALENCE (CTV1S5(1), ADEV)
         EQUIVALENCE (CTV1S5(2), CDEV)
         EQUIVALENCE (CTV1S5(3), MONDEV)
         EQUIVALENCE (CTV1S5(4), MICRO)
         EQUIVALENCE (CTV1S5(5), MSPEED)
         EQUIVALENCE (CTV1S5(6), IERDEV)
         EQUIVALENCE (CTV1S5(7), ERRDEV)
         EQUIVALENCE (CTV1S5(8), INDEV1)
         EQUIVALENCE (CTV1S5(9), INDEV2)
         EQUIVALENCE (CTV1S5(10),ISEM1)
         EQUIVALENCE (CTV1S5(11),ISEM2)
         EQUIVALENCE (CTV1S5(12),ISEM3)


C  Section 6 : Telescope and fundamental constants
C  -----------------------------------------------

       INTEGER*2   CTV1S6(40)
       EQUIVALENCE (CTV1S6(1),CTV1ALL(1241))

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
         EQUIVALENCE (CTV1S6(1), PI)
         EQUIVALENCE (CTV1S6(5), TWOPI)
         EQUIVALENCE (CTV1S6(9), RADPS)
         EQUIVALENCE (CTV1S6(13),RADPD)
         EQUIVALENCE (CTV1S6(17),C)
         EQUIVALENCE (CTV1S6(21),FROBS)
         EQUIVALENCE (CTV1S6(25),FROSC)
         EQUIVALENCE (CTV1S6(29),PCFAC)
         EQUIVALENCE (CTV1S6(33),USPAC)
         EQUIVALENCE (CTV1S6(37),SPFAC)


C  Section 7 : Current observation information
C  -------------------------------------------

       INTEGER*2  CTV1S7(80)
       EQUIVALENCE (CTV1S7(1),CTV1ALL(1401))
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
         EQUIVALENCE (CTV1S7(1), TITLE)
         EQUIVALENCE (CTV1S7(9), SOURCE)
         EQUIVALENCE (CTV1S7(17),RAMC)
         EQUIVALENCE (CTV1S7(21),DECMC)
         EQUIVALENCE (CTV1S7(25),DATOBS)
         EQUIVALENCE (CTV1S7(29),RAREF)
         EQUIVALENCE (CTV1S7(33),DECREF)
         EQUIVALENCE (CTV1S7(37),DATREF)
         EQUIVALENCE (CTV1S7(41),STHA)
         EQUIVALENCE (CTV1S7(45),NSP)
         EQUIVALENCE (CTV1S7(47),INTSAM)
         EQUIVALENCE (CTV1S7(49),NSAMP)
         EQUIVALENCE (CTV1S7(51),NCHOP)
         EQUIVALENCE (CTV1S7(53),NBAD)
         EQUIVALENCE (CTV1S7(55),IDAT1)
         EQUIVALENCE (CTV1S7(58),IDAT2)
         EQUIVALENCE (CTV1S7(61),ITIM1)
         EQUIVALENCE (CTV1S7(64),ITIM2)
         EQUIVALENCE (CTV1S7(67),ISTIM1)
         EQUIVALENCE (CTV1S7(70),ISTIM2)


C  Section 8 : Control tables source packing information.
C  ------------------------------------------------------

       INTEGER*4   CTV1S8(20)
       EQUIVALENCE (CTV1S8(1),CTV1ALL(1441))

C     Mapping of record outined in /mrao/post/include/ctab_pack.inc
C
         INTEGER*4    CTV1_PACK(16)

         EQUIVALENCE (CTV1S8(1),CTV1_PACK(1))


C  Section 9A: Remove definitions
C  ------------------------------

       INTEGER*4     CTV1_REMDEF(1616)
       EQUIVALENCE ( CTV1_REMDEF(1), CTV1ALL(1457) )

C     Mapping of the records outlined in /mrao/post/include/src_pack.inc
C     and /mrao/post/include/remove_record.inc, for each remove source.
C     This section is included in the :REM files (ct type 2) and makes
C     the total length of the control tables up to 6 pages.

C  Section 9B: Gain definitions
C  ----------------------------

       INTEGER*4     CTV1_GAINDEF(1616)
       EQUIVALENCE ( CTV1_GAINDEF(1), CTV1ALL(1457) )

C     Mapping of the records outlined in /mrao/post/include/src_pack.inc
C     and /mrao/post/include/cal_record.inc, for each calibration source.  
C     This section is included in the :CAL files (ct type 3) and makes
C     the total length of the control tables up to 6 pages.

