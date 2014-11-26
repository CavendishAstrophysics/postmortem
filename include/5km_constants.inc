*
*     5KM-CONSTANTS:INCL
*
* last modified 25 July 91
C
C     This file contains constants which are fundamental to the
C     definition of the Cambridge 5km telescope.
C
C     These are required in the definition of the telescope control
C     tables and are $INCLUDEd in the file 5KM-CT:INCL.
C
C
      INTEGER  MAX_AES, MAX_SPAC, MAX_SAMP, MAX_SUBB, MAX_CHANNEL
      INTEGER  MAX_CENTRE, maxcentre
C
      PARAMETER ( MAX_AES     = 8 )
      PARAMETER ( MAX_SPAC    = 28 )
      PARAMETER ( MAX_SAMP    = 3000 )
      PARAMETER ( MAX_SUBB    = 5 )
      PARAMETER ( MAX_CHANNEL = 8 )
      PARAMETER ( MAX_CENTRE  = 8, maxcentre = MAX_CENTRE)
C


* observing positions (quarter-spacings)

      integer*2 optable(36)/4,7,8,9,10,12,16,20,24,28,32,36,40,
     #                     44,48,52,56,60,64,68,72,76,80,84,88,92,
     #                     96,100,104,108,112,116,120,124,128,132/

