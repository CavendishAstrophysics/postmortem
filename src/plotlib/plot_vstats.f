C+PLOT_VSTATS

      SUBROUTINE PLOT_VSTATS( N, A, STATS, POSNS )
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
C     DJT    20 May 1993
C
C-
      REAL*8      SUM, SUM_SQR
      INTEGER     I, NN

      STATS(1) = 0.0
      STATS(2) = 0.0
      STATS(3) = 0.0
      STATS(4) = 0.0

      IF (N .GE. 1) THEN

          NN = 0
          DO 100, I = 1, N
              IF (A(I) .NE. 0.0) THEN
                 NN = NN + 1
                 IF (NN.EQ.1) THEN
                   STATS(1) = A(I)
                   STATS(2) = A(I)
                   POSNS(1) = I
                   POSNS(2) = I
                   SUM      = 0.0
                   SUM_SQR  = 0.0
                 END IF

                 SUM = SUM + A(I)
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

          IF (NN.GT.0) THEN
             STATS(3) = SUM/REAL(NN)
             STATS(4) = SUM_SQR/REAL(NN)-STATS(3)*STATS(3)
             STATS(4) = SQRT(AMAX1(0.0, STATS(4)))
          END IF
      END IF

      RETURN
      END



