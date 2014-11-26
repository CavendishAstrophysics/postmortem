C  Error status values for the interactive data editor.
C
C     < error description >
C         INTEGER  <error name>
C         PARAMETER ( <error name> = -80nn )

C     Write new data on exit from ide_system.
          INTEGER         IDE_WRITE
          PARAMETER     ( IDE_WRITE = -9001 )

C     Internal work array too small for current operation.
          INTEGER         IDE_ARR_TOOSMALL
          PARAMETER     ( IDE_ARR_TOOSMALL = -9002 )

C     Plot boundary overlap
          INTEGER         IDE_PLOTOVERLAP
          PARAMETER     ( IDE_PLOTOVERLAP = -9003 )
