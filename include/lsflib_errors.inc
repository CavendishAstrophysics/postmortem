C  Error status values for LSFLIB library.
C
C     < error description >
C         INTEGER  <error name>
C         PARAMETER ( <error name> = -10nn )

C     Illegal or corrupted logical sample file
          INTEGER         ILL_LSF
          PARAMETER     ( ILL_LSF = -3001 )

C     No logical sample files saved for this sample file.
          INTEGER         NO_LSFSAVED
          PARAMETER     ( NO_LSFSAVED = -3002 )

C     Illegal logical sample file buffer requested
          INTEGER         ILL_BUFFER
          PARAMETER     ( ILL_BUFFER  = -3003 )

C     Logical sample file buffer size conflict.
          INTEGER         ILL_BUFFSIZE
          PARAMETER     ( ILL_BUFFSIZE= -3004 )

C     Illegal flag parameter when setting spacing list
          INTEGER         ILL_FLAG
          PARAMETER     ( ILL_FLAG   = -3005 )

C     Illegal smoothing type
          INTEGER         ILL_SMOOTH
          PARAMETER     ( ILL_SMOOTH = -3006 )

C     Illegal smoothing control flag value
          INTEGER         ILL_SMOOTHFLG
          PARAMETER     ( ILL_SMOOTHFLG = -3007 )

C     Illegal interference chop type
          INTEGER         ILL_INTCHOP
          PARAMETER     ( ILL_INTCHOP = -3008 )

C     Illegal gain correction type.
          INTEGER         ILL_GAINS
          PARAMETER     ( ILL_GAINS = -3009 )

C     Too many sources for LSF model visibilities definition.
          INTEGER         ILL_NUMSRCS
          PARAMETER     ( ILL_NUMSRCS = -3010 )

C     Telescope not defined for use with logical sample files.
          INTEGER         ILL_LSF_TSCOPE
          PARAMETER     ( ILL_LSF_TSCOPE = -3011 )

C     Bad model definition for logical sample file.
          INTEGER         ILL_MODEL
          PARAMETER     ( ILL_MODEL = -3012 )

C     Invalid telescope bandwidth type
          INTEGER         ILL_BANDTYPE
          PARAMETER     ( ILL_BANDTYPE = -3013 )
