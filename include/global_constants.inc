C***********************************************************************
C
C     GLOBAL_constants.inc
C
C***********************************************************************
C
C     This file contains constants fundamental to the definition of
C     telescopes processed via the POSTMORTEM program.  These values
C     are used within POSTMORTEM for setting up workspace arrays, and
C     are the maxima of the corresponding values for all telescopes.
C
C     The appropriate values for RT and CLFST telescopes are:
C
C     Parameter          RT            CLFST
C     ---------          --            -----
C      MAX_AES            8               60
C      MAX_RT_AES         8                -
C      MAX_HUTS           -                8
C      MAX_CENTRE         8                1
C      MAX_SUBB           5                1
C      MAX_CHANNEL        8                1
C      MAX_SPAC          28              800
C      MAX_VIS         1120              800
C
C
      INTEGER  MAX_AES, MAX_HUTS, MAX_CENTRE, MAX_SAMP
      INTEGER  MAX_SUBB, MAX_CHANNEL, MAX_SPAC, MAX_VIS
      INTEGER  MAX_RT_AES
C
      PARAMETER ( MAX_AES     = 60 )      ! no.of aerials
      PARAMETER ( MAX_RT_AES  = 8  )      ! no.of aerials on RT
      PARAMETER ( MAX_HUTS    = 8 )       ! no.of huts
      PARAMETER ( MAX_CENTRE  = 8 )       ! no.of observing centres
      PARAMETER ( MAX_SUBB    = 5 )       ! no.of sub-bands
      PARAMETER ( MAX_CHANNEL = 8 )       ! no.of channels per sub-band
      PARAMETER ( MAX_SPAC    = 800 )     ! no of spacings
      PARAMETER ( MAX_VIS     = 1120 )    ! no.of visibilities
      PARAMETER ( MAX_SAMP    = 6000 )    ! no.of samples
