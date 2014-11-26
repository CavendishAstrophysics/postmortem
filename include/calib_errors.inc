C  Error status values for POSTMORTEM system
C
C     < error description >
C         INTEGER  <error name>
C         PARAMETER ( <error name> = -60nn )

C     Illegal calibration type
          INTEGER         ILL_CALIBRATION
          PARAMETER     ( ILL_CALIBRATION = -6001 )
C     Matrices not initialised
          INTEGER         ILL_MATINIT
          PARAMETER     ( ILL_MATINIT     = -6002 )
C     Error in call to NAG
          INTEGER         ILL_CALNAG
          PARAMETER     ( ILL_CALNAG      = -6003 )
