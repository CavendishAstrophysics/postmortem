
C     *****************************************************************
*+ENposobsdat

       SUBROUTINE ENposobsdat ( en_raobs, en_decobs, en_obsdat, status )
C      ----------------------------------------------------------
C
C  Returns map centre coordinates from redtape.  Should be in MAPLIB.
C
C  Returned:
C      EN_raobs    real*8      RA of map centre (radians)
C      EN_decobs   real*8      Dec of map centre (radians)
C      EN_OBSDAT   real*8      date of observation (years)
C      STATUS      integer     status value
C
C  The coordinates of the map centre are returned from 'astronomical'
C  redtape (section 4), expressed date of observation.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (PJW 2/10/91)
C
*-
      REAL*8     EN_raobs, EN_decobs, EN_OBSDAT
      INTEGER    STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
      IF (STATUS .NE. 0) RETURN
C
      EN_raobs  = raobs
      EN_decobs = decobs
      EN_OBSDAT = OBSDAT
C
      return

      END
