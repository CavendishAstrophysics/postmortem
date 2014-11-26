C***********************************************************************
C
C  /mrao/post/include/control_tab_v1.inc
C
C  CAMBRIDGE SYNTHESIS TELESCOPES CONTROL TABLES (RYLE Telescope)
C  --------------------------------------------------------------
C
C  Last updated (version A9)   19/4/90 by DJT
C
C  These control tables contain the parameters that are used to define and
C  control the telescopes during an observing run.  They are also used to
C  define calibration and correction information determined either during
C  the run or afterwards.
C
C  This file describes control tables Version 2 for the POSTMORTEM program.
C
C  The control tables can be used in two ways:
C  1.  If they are to be used locally to a routine then pre-declare
C      the integer variable CT_ALL.
C  2.  If they are to be used globally then pre-include the file
C      (postmortem)control-tables:incl which declares the named
C      common area /CONTROL_TABLES/.
C
C  The following are appropriate dimensions (the equivalent dimensions for
C  the CLFST are also given).
C
C     Parameter        5-km            CLFST
C     ---------        ----            -----
C      MAX_AES            8               60
C      MAX_SPAC          28              800
C      MAX_HUTS                            8
C      MAX_SUBB           5                1
C      MAX_CHANNEL        8                1
C      MAX_CENTRE         8                1
C
C  They are defined in the include file 5KM-CONSTANTS:INCL.
C
C***********************************************************************
C
       INTEGER      CTV2LEN
       PARAMETER   (CTV2LEN = 512*8)
C
       INTEGER*4    CTV2ALL(CTV2LEN)
       EQUIVALENCE (CTV2ALL(1), CT_ALL)
C
C
	include '/mrao/post/include/5km_constants.inc'
C
C  Section 1 : Text
C  ----------------

       INTEGER      POINTER_CT1
       PARAMETER   (POINTER_CT1 = 1)
       INTEGER      LENGTH_CT1
       PARAMETER   (LENGTH_CT1 = 112)
C
       INTEGER*4    ICT1(LENGTH_CT1)
       EQUIVALENCE (ICT1(1), CTV2ALL(POINTER_CT1))

C     Archive status flag

       INTEGER*4     SAVE_FLAG

C     Archive tape volume name, user name of most recent access
C
       CHARACTER*6   SAVE_VOLUME
       CHARACTER*16  LAST_USER

C     Textual comment lines set by the observer/operator.
C
       CHARACTER*80  COMMENT_LINES(5)

C     Patch level indicator, bits are set for the patches defined
C     in the include file (POSTMORTEM)PATCHES-TAB-V2:INCL.
C
       INTEGER*4     PATCH_LEVEL
C
       EQUIVALENCE  (ICT1(1),   SAVE_FLAG)
       EQUIVALENCE  (ICT1(2),   SAVE_VOLUME)
       EQUIVALENCE  (ICT1(4),   LAST_USER)
       EQUIVALENCE  (ICT1(8),   COMMENT_LINES)
       EQUIVALENCE  (ICT1(108), PATCH_LEVEL)


C
C  Section 2 : Control tables packing information
C  ----------------------------------------------

       INTEGER      LENGTH_CT2
       PARAMETER   (LENGTH_CT2 = 16)
       INTEGER      POINTER_CT2
       PARAMETER   (POINTER_CT2 = POINTER_CT1 + LENGTH_CT1)
C
       INTEGER*4    ICT2(LENGTH_CT2)
       EQUIVALENCE (ICT2(1), CTV2ALL(POINTER_CT2))
C
C     Mapping of the record outlined in (postmortem)ctab-pack:incl
C
       INTEGER*4    CTV2_PACK(16)
       EQUIVALENCE (ICT2(1), CTV2_PACK(1))


C
C  Section 3 : Current observation parameters
C  ------------------------------------------

       INTEGER      LENGTH_CT3
       PARAMETER   (LENGTH_CT3 = 256)
       INTEGER      POINTER_CT3
       PARAMETER   (POINTER_CT3 = POINTER_CT2 + LENGTH_CT2)
C
       INTEGER*4    ICT3(LENGTH_CT3)
       EQUIVALENCE (ICT3(1), CTV2ALL(POINTER_CT3))
C
C     Include the standard track Q definition used in the RT system.
C     This defines the current sample file observation.
C
	include '/mrao/post/include/ryle_q_item.inc'
C
       EQUIVALENCE (ICT3(1), QITEM(0))


C
C  Section 4 : Current observation, derived quantities
C  ---------------------------------------------------

       INTEGER      LENGTH_CT4
       PARAMETER   (LENGTH_CT4 = 32)
       INTEGER      POINTER_CT4
       PARAMETER   (POINTER_CT4 = POINTER_CT3 + LENGTH_CT3)
C
       INTEGER*4    ICT4(LENGTH_CT4)
       EQUIVALENCE (ICT4(1), CTV2ALL(POINTER_CT4))
C
C     The quantities detailed in this section are useful for reference
C     and display purposes.   The parameters of Section 2 should be used
C     for all astrometric calculations.

C     Reference date for RAref, DECref expressed as decimal year
C
       REAL*8       DATREF

C     Date at midday of the observation expressed as decimal year
C
       REAL*8       DATOBS

C     Date (day,month,year) at beginning and end of run
C
       INTEGER*4    IDAT1(3), IDAT2(3)

C     Local time (secs,mins,hours) at beginning and end of run
C
       INTEGER*4    ITIM1(3), ITIM2(3)

C     Sidereal time (secs,mins,hours) at beginning and end of run
C
       INTEGER*4    ISTIM1(3), ISTIM2(3)

C     Run in progress flag, set on acquisition of first sample
C
       INTEGER*4    ISTART

C     Run completed flag, set on normal termination of observing run
C
       INTEGER*4    ISTOP

C     Offset of local time with respect to UT in hours
C
       INTEGER*4    IUTIM

C
       EQUIVALENCE (ICT4(1), DATREF)
       EQUIVALENCE (ICT4(3), DATOBS)
       EQUIVALENCE (ICT4(5), IDAT1)
       EQUIVALENCE (ICT4(8), IDAT2)
       EQUIVALENCE (ICT4(11),ITIM1)
       EQUIVALENCE (ICT4(14),ITIM2)
       EQUIVALENCE (ICT4(17),ISTIM1)
       EQUIVALENCE (ICT4(20),ISTIM2)
       EQUIVALENCE (ICT4(23),ISTART)
       EQUIVALENCE (ICT4(24),ISTOP)
       EQUIVALENCE (ICT4(25),IUTIM)


C
C  Section 5 : Telescope configuration information
C  -----------------------------------------------

       INTEGER      LENGTH_CT5
       PARAMETER   (LENGTH_CT5 = 64)
       INTEGER      POINTER_CT5
       PARAMETER   (POINTER_CT5 = POINTER_CT4 + LENGTH_CT4)
C
       INTEGER*4    ICT5(LENGTH_CT5)
       EQUIVALENCE (ICT5(1), CTV2ALL(POINTER_CT5))
C
C     Telescope Identifier. The telescopes currently recognised are
C     T151, T38, VLBI (1,2,3) and RYLE (4 and 5). See also RTELE in the
C     map redtape definition (extended to non Cambridge telescopes).
C
       INTEGER*4    ITSCOPE

C     Array giving the 5km aerial designation for each of the NAES
C     aerials. The possible values are 1-8.

       INTEGER*2    IAE_CODE(MAX_AES)

C     Array giving the 5km spacing designation for each entry in the
C     spacing list. The unit used is 1/4 spacing thus the separation
C     between the fixed aerials (spacing 32) is recorded as 128 in this
C     array.
C
       INTEGER*2    ISP_CODE(MAX_SPAC)

C     Array giving the 5-km sub-band designations for each of the NSUBB
C     sub-bands. They are stored as an integer array with designations
C     1-5 corresponding to A-E inclusive.

       INTEGER*2    IBA_CODE(MAX_SUBB)

C     Array giving the 5-km channel designations for each of the NCHANNEL
C     frequency channels.  They are stored as an integer array with
C     designations 1-8 inclusive.

       INTEGER*2    ICH_CODE(MAX_CHANNEL)

C     Array containing the indexes for east and west aerials for
C     each spacing in the current configuration.  The entries are
C     ordered (but NOT indexed) according to spacing.
C
       INTEGER*2    ISPAE(2, MAX_SPAC)
C
       EQUIVALENCE (ICT5(1),    ITSCOPE)
       EQUIVALENCE (ICT5(2),    IAE_CODE)
       INTEGER      IP50
       PARAMETER   (IP50 = 2+MAX_AES/2 + 1)
       EQUIVALENCE (ICT5(IP50), ISP_CODE)
       INTEGER      IP51
       PARAMETER   (IP51 = IP50+MAX_SPAC/2 + 1)
       EQUIVALENCE (ICT5(IP51), IBA_CODE)
       INTEGER      IP52
       PARAMETER   (IP52 = IP51+MAX_SUBB/2 + 1)
       EQUIVALENCE (ICT5(IP52), ICH_CODE)
       INTEGER      IP53
       PARAMETER   (IP53 = IP52+MAX_CHANNEL/2 + 1)
       EQUIVALENCE (ICT5(IP53), ISPAE)


C
C  Section 6 : Telescope parameters (including geometry)
C  -----------------------------------------------------

       INTEGER      LENGTH_CT6
       PARAMETER   (LENGTH_CT6 = 512)
       INTEGER      POINTER_CT6
       PARAMETER   (POINTER_CT6 = POINTER_CT5 + LENGTH_CT5)
C
       INTEGER*4    ICT6(LENGTH_CT6)
       EQUIVALENCE (ICT6(1), CTV2ALL(POINTER_CT6))
C
C     Include the telescope parameter block used in the RT system,
C     containing constants and slowly varying telescope parameters.
C
	include '/mrao/post/include/ryle_tel_pars.inc'
C
       EQUIVALENCE (ICT6(1), TPA(1))


C
C  Section 7 : Telescope geometry, derived quantities
C  --------------------------------------------------

       INTEGER      LENGTH_CT7
       PARAMETER   (LENGTH_CT7 = 6*MAX_CHANNEL*MAX_SUBB*MAX_AES)
       INTEGER      POINTER_CT7
       PARAMETER   (POINTER_CT7 = POINTER_CT6 + LENGTH_CT6)
C
       INTEGER*4    ICT7(LENGTH_CT7)
       EQUIVALENCE (ICT7(1), CTV2ALL(POINTER_CT7))
C
C     X, Y, and Z coordinates of each aerial in wavelengths. The
C     coordinate system is centred near the telescope centre and is
C     aligned so that:
C
C         Z   is in the direction of the North Celestial Pole
C         X   is in perpendicular to Z and in the plane defined
C             by the Z axis and the 5 km datum line.
C         Y   is perpendicular to both.
C
       REAL*8       X(MAX_CHANNEL, MAX_SUBB, MAX_AES),
     :              Y(MAX_CHANNEL, MAX_SUBB, MAX_AES),
     :              Z(MAX_CHANNEL, MAX_SUBB, MAX_AES)

       INTEGER      POINTER_X, POINTER_Y, POINTER_Z

       PARAMETER   (POINTER_X = 1)
       PARAMETER   (POINTER_Y = POINTER_X + LENGTH_CT7/3)
       PARAMETER   (POINTER_Z = POINTER_Y + LENGTH_CT7/3)

       EQUIVALENCE (ICT7(POINTER_X), X)
       EQUIVALENCE (ICT7(POINTER_Y), Y)
       EQUIVALENCE (ICT7(POINTER_Z), Z)


C
C  Section 8 : Telescope calibration parameters
C  --------------------------------------------

       INTEGER      LENGTH_CT8
       PARAMETER   (LENGTH_CT8 = 32)
       INTEGER      POINTER_CT8
       PARAMETER   (POINTER_CT8 = POINTER_CT7 + LENGTH_CT7)
C
       INTEGER*4    ICT8(LENGTH_CT8)
       EQUIVALENCE (ICT8(1), CTV2ALL(POINTER_CT8))

C     Textual definition of the units of the sample file
C
       CHARACTER*8  AUNITS

C     Scales data from correlator to external units as defined by AUNITS.
C
       REAL*4       AMPSCL

C     Cutoff amplitude during run in internal units. To provide
C     maximum resolution in the data stored on the sample file, the
C     following scaling factor is applied before writing the data to
C     file as integer*2's : AFAC = 32767/(ACHOP*AMPSCL).
C
       REAL*4       ACHOP

       EQUIVALENCE (ICT8(1),   AUNITS)
       EQUIVALENCE (ICT8(3),   AMPSCL)
       EQUIVALENCE (ICT8(4),   ACHOP)


C
C  Section 9A : Source packing information
C  ---------------------------------------

       INTEGER      LENGTH_CT9A
       PARAMETER   (LENGTH_CT9A = MAX_CENTRE*16)
       INTEGER      POINTER_CT9
       PARAMETER   (POINTER_CT9 = POINTER_CT8 + LENGTH_CT8)
C
       INTEGER*4    ICT9A(LENGTH_CT9A)
       EQUIVALENCE (ICT9A(1), CTV2ALL(POINTER_CT9))
C
C     Mapping of the record outlined in (postmortem)src-pack:incl,
C     for each of MAX_CENTRE centres.  This section is included in
C     the physical sample files containing observed visibilities,
C     (ct type 1) and makes the total length of control tables up
C     to 6 pages.
C

C-----------------------------------------------------------------------

C
C  Section 9B : Remove source definitions
C  --------------------------------------

       INTEGER      LENGTH_CT9B
       PARAMETER   (LENGTH_CT9B = 1152)
C
       INTEGER*4    ICT9B(LENGTH_CT9B)
       EQUIVALENCE (ICT9B(1), CTV2ALL(POINTER_CT9))
C
C     Mapping of the records outlined in (postmortem)src-pack:incl
C     and (postmortem)remove-record:incl, for each remove source.
C     This section is included in the :REM files (ct type 2) and
C     makes the total length of control tables up to 8 pages.
C

C-----------------------------------------------------------------------

C
C  Section 9C : Calibration source definitions
C  -------------------------------------------

       INTEGER      LENGTH_CT9C
       PARAMETER   (LENGTH_CT9C = 1152)
C
       INTEGER*4    ICT9C(LENGTH_CT9C)
       EQUIVALENCE (ICT9C(1), CTV2ALL(POINTER_CT9))
C
C     Mapping of the records outlined in (postmortem)src-pack:incl
C     and (postmortem)cal-record:incl, for each calibration source.
C     This section is included in the :CAL files (ct type 3) and
C     makes the total length of control tables up to 8 pages.
C


C-----------------------------------------------------------------------
C
C  Control Table Definition 5km Telescope END
C
C-----------------------------------------------------------------------
