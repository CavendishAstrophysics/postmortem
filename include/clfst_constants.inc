C***********************************************************************
C
C     /mrao/post/include/clfst_constants.inc
C
C***********************************************************************
C
C     This file contains constants which are fundamental to the
C     definition of the Cambridge Low Frequency Synthesis
C     Telescope (CLFST). They are needed for the definition
C     of the control tables and so are 'include'd in the file
C     control_tables.inc. They are only for reference
C     within the SAMPLIB library, and some POSTMORTEM routines.
C
C     Maximum number of huts, aerials, spacings, samples - the latter
C     for dimensioning POSTMORTEM arrays.
C
      INTEGER MAX_HUTS, MAX_AES, MAX_SPAC, MAX_SAMP
      PARAMETER ( MAX_HUTS = 8 )
      PARAMETER ( MAX_AES  = 60 )
      PARAMETER ( MAX_SPAC = 800 )
      PARAMETER ( MAX_SAMP = 3000 )
C
C     Number of huts, aerials in East and West arrays
C
      INTEGER NEAST_HUTS, NEAST_AES, NWEST_HUTS, NWEST_AES
      PARAMETER ( NEAST_HUTS = 4 )
      PARAMETER ( NEAST_AES  = 28 )
      PARAMETER ( NWEST_HUTS = 4 )
      PARAMETER ( NWEST_AES  = 32 )

