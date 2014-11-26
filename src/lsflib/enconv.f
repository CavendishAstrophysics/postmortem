
*+ENconv

      SUBROUTINE ENconv ( en_convtp, en_convos, en_convhw,
     *                     en_convpa, status )
C      ----------------------------------------------------------
C
C  Returns parameters of griding convolution from redtape.
C                                             Should be in MAPLIB.
C
C  Returned:
C      EN_convtp      integer     convolution type
C      EN_convos      integer     over-sampling
C      EN_convhw      integer     half-width
C      EN_convpa(2)   real        parameters
C      STATUS      integer     status value
C
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (PJW 9/3/92)
C
*-
      REAL       en_convpa(2)
      INTEGER    en_convtp, en_convos, en_convhw,  STATUS
C
      include '/mrao/include/maplib_redtape.inc'
C
      IF (STATUS .NE. 0) RETURN
C
      EN_convtp = convtp
      EN_convos = convos
      EN_convhw = convhw
      en_convpa(1) = convpa(1)
      en_convpa(2) = convpa(2)
C
      return

      END
