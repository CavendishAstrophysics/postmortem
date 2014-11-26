C
C+buffer_wt

      subroutine buffer_wt( n, a, buff_wt )

C     Returns the weight for a given buffer in the mapping process.

C     Given
C         Number of buffer elements
              integer     n
C         Visibility buffer
              complex     a(n)

C     Returned
C         Weight to be given to the buffer
              real*8      buff_wt

C     The buffer weight returned is basically the inverse of the mean
C     of the variance of the cos and sin channels taken independently.
C     Null (0,0) elements are ignored. If there is less than two valid
C     elements in the array the buffer_wt is returned as zero.
C
C     NPR    18 May 1988
C
C-
      real*8      rsum, isum, rsum_sqr, isum_sqr, variance
      integer     i, non_null

      rsum     = 0.0D+0
      rsum_sqr = 0.0D+0
      isum     = 0.0D+0
      isum_sqr = 0.0D+0
      non_null = 0

      do 100, i = 1, n
          if (a(i).ne.(0.0,0.0)) then
              rsum     = rsum     + real(a(i))
              rsum_sqr = rsum_sqr + real(a(i))*real(a(i))
              isum     = isum     + imag(a(i))
              isum_sqr = isum_sqr + imag(a(i))*imag(a(i))
              non_null = non_null + 1
          end if
  100 continue

      if (non_null .ge. 2) then
          variance = ((rsum_sqr+isum_sqr) -
     *                (rsum*rsum+isum*isum)/real(non_null)) /
     *                                            (2*real(non_null))
          if (variance .le. 0.0) then
C             Suspicious - reject buffer
              buff_wt = 0.0D+0
          else
              buff_wt = 1.0 / variance
          end if
      else
          buff_wt = 0.0D+0
      end if

      return
      end
