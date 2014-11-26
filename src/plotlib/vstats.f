C+VSTATS

      SUBROUTINE VSTATS( N, A, STATS, POSNS )
C
C     Returns statistics on a vector.
C
C     Given
C         Number of vector elements
              INTEGER     N
C         Vector
              REAL        A(N)

C     Returned
C         Minimum, maximum, mean and standard deviation of the vector.
              REAL        STATS(4)
C         Position of the minimum and maximum in the vector
              INTEGER     POSNS(2)

C     If the array has only one element, the standard deviation is
C     zero, and all the other statistics refer to the only element.
C     If N is less than one the routine is a null routine.
C
C     Note that this routine ignores data equal to zero.
C
C     NPR     4 Feb 1988
C     DJT     6 Oct 1992
C
C-
      REAL*8      SUM, SUM_SQR
      INTEGER     I, NN

      IF (N .GE. 1) THEN
          STATS(1) = A(1)
          STATS(2) = A(1)
          POSNS(1) = 1
          POSNS(2) = 1
          SUM      = A(1)
          SUM_SQR  = A(1)*A(1)
          NN       = 1

          DO 100, I = 2, N
              IF (A(I) .NE. 0.0) THEN
                 NN      = NN      + 1
                 SUM     = SUM     + A(I)
                 SUM_SQR = SUM_SQR + A(I)*A(I)
                 IF (A(I) .LT. STATS(1)) THEN
                     STATS(1) = A(I)
                     POSNS(1) = I
                 END IF
                 IF (A(I) .GT. STATS(2)) THEN
                     STATS(2) = A(I)
                     POSNS(2) = I
                 END IF
             END IF
  100     CONTINUE

          STATS(3) = SUM/REAL(NN)
          STATS(4) = SUM_SQR/REAL(NN)-STATS(3)*STATS(3)
          STATS(4) = SQRT(AMAX1(0.0, STATS(4)))
      END IF

      RETURN
      END

