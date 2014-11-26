C  /mrao/post/include/samplib_errors.inc
C
C  'include file containing error codes produced by the SAMPLIB library
C
C     Illegal sample file open access type
       INTEGER     ILL_ACCESS
       PARAMETER ( ILL_ACCESS = -2001 )

C     Illegal sample file open report flag
       INTEGER     ILL_REPFLG
       PARAMETER ( ILL_REPFLG = -2002 )

C     No source with this number in this sample file
       INTEGER     NO_SRCNUM
       PARAMETER ( NO_SRCNUM = -2003 )

C     Too many sample files
       INTEGER     TOO_MANYSF
       PARAMETER ( TOO_MANYSF = -2004 )

C     Sample file packing parameters not saved
       INTEGER     SF_NOTSAV
       PARAMETER ( SF_NOTSAV = -2005 )

C     Illegal unweighted/weighted sample processing flag
       INTEGER     ILL_PROCFLG
       PARAMETER ( ILL_PROCFLG = -2006 )

C     Illegal sample file data type code
       INTEGER     ILL_DATATYPE
       PARAMETER ( ILL_DATATYPE = -2007 )

C     Illegal aerial list
       INTEGER     ILL_AELIST
       PARAMETER ( ILL_AELIST = -2008 )

C     Illegal hut list
       INTEGER     ILL_HUTLIST
       PARAMETER ( ILL_HUTLIST = -2009 )

C     Illegal spacing list
       INTEGER     ILL_SPLIST
       PARAMETER ( ILL_SPLIST = -2010 )

C     Sample number out of range
       INTEGER     ILL_SAMPLE
       PARAMETER ( ILL_SAMPLE = -2011 )

C     No such source in source list
       INTEGER     NO_SOURCE
       PARAMETER ( NO_SOURCE = -2012 )

C     Ionospheric correction not available
       INTEGER     NO_IONCORR
       PARAMETER ( NO_IONCORR = -2013 )

C     Both cos and sin are zero in ionospheric correction
       INTEGER     ILL_IONCORR
       PARAMETER ( ILL_IONCORR = -2014 )

C     File is not a remove file
       INTEGER     NOT_REMFILE
       PARAMETER ( NOT_REMFILE = -2015 )

C     Illegal/incorrect control tables in sample file
       INTEGER     ILL_CONTTAB
       PARAMETER ( ILL_CONTTAB = -2016 )

C     Illegal sample file packing parameters
       INTEGER     ILL_SFPACK
       PARAMETER ( ILL_SFPACK = -2017 )

C     Sample file read buffer length exceeded
       INTEGER     NO_SFBUFFER
       PARAMETER ( NO_SFBUFFER = -2018 )

C     Bad sample
       INTEGER     BAD_SAMPLE
       PARAMETER ( BAD_SAMPLE = -2019 )

C     File is not a physical sample file
       INTEGER     NOT_PHYSF
       PARAMETER ( NOT_PHYSF = -2020 )

C     File is not a gains correction file
       INTEGER     NOT_GAINFILE
       PARAMETER ( NOT_GAINFILE = -2021 )

C     Too many sources for ionospheric correction
       INTEGER     TOO_MANYSRCS
       PARAMETER ( TOO_MANYSRCS = -2022 )

C     Illegal file type
       INTEGER     ILL_FILETYPE
       PARAMETER ( ILL_FILETYPE = -2023 )

C     Routines not implemented for this telescope
       INTEGER     ILL_TSCOPE
       PARAMETER ( ILL_TSCOPE = -2024 )

C     Illegal merge type
       INTEGER     ILL_MERGE
       PARAMETER ( ILL_MERGE = -2025 )

C     Illegal aerial index
       INTEGER     ILL_AERIAL
       PARAMETER ( ILL_AERIAL = -2026 )

C     Illegal spacing index
       INTEGER     ILL_SPAC
       PARAMETER ( ILL_SPAC = -2027 )

C     Illegal sub-band index
       INTEGER     ILL_SUBBAND
       PARAMETER ( ILL_SUBBAND = -2028 )

C     Illegal channel index
       INTEGER     ILL_CHANNEL
       PARAMETER ( ILL_CHANNEL = -2029 )

C     Illegal visibility index
       INTEGER     ILL_VIS
       PARAMETER ( ILL_VIS = -2030 )

C     Zero-corrections not available
       INTEGER     NO_ZCORR
       PARAMETER ( NO_ZCORR = -2031 )
