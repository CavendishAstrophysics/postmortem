C
C     *****************************************************************
C
C+int_chop
C
      SUBROUTINE int_chop  ( num_vis,
     *                       vis,
     *                       int_chop_type,
     *                       int_chop_params,
     *                       pre_post_index,
     *                       s                      )

C
C     Applies interference chopping to a visibility buffer
C
C     Given:
C         Number of spacings.
              integer*4       num_vis
C         Visibility buffer
              real*4          vis( 2, num_vis )
C         Interference chop type
              integer*4       int_chop_type
C         Interference chop parameters
              integer*4       int_chop_params(10)
C         pre or post array index (type 5 only)
              integer*4       pre_post_index

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Applies interference chopping of various sorts to the visibility
C     buffer.
C
C     Type 5  - multi level clipping -              added by PJW 19/9/90
C     type 6 -  as type 3 except var(cos) +  var(sin) tested
C               rather than      var(cos) .or. var(sin)       PJW 301090
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/int_chop_record.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer*4       i, j
C         Temporary store and summation variables
              integer         n
              real            cos_sqr, sin_sqr, max_amp_sqr
              real*8          sum_cos, sum_sin, ssq_cos, ssq_sin
              real*8          var_cos, var_sin, var_limit
C         clipping level interp variables
              real*4          level, level1, level2
              integer*4       spacing1, spacing2
C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Copy interference chop parameters to definition record
      int_chop_record(1) = int_chop_params(1)
      int_chop_record(2) = int_chop_params(2)

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( int_chop_type .eq. 0 ) then
          continue

      else if ( int_chop_type .eq. 1 ) then
          do i = 1, num_vis
              if ( abs( vis( 1, i ) ) .gt. int_max_signal .or.
     *             abs( vis( 2, i ) ) .gt. int_max_signal      ) then
                  vis( 1, i ) = 0.0
                  vis( 2, i ) = 0.0
              end if
          end do

      else if ( int_chop_type .eq. 2 ) then
          max_amp_sqr = max_signal*max_signal
          n = 0

          do i = 1, num_vis
              if ((vis(1,i)*vis(1,i)+vis(2,i)*vis(2,i)) .gt.
     *                                        max_amp_sqr ) then
                  vis( 1, i ) = 0.0
                  vis( 2, i ) = 0.0
              end if
              if (vis(1,i).eq.0.0 .and. vis(2,i).eq.0.0) n = n+1
          end do

          if ((n*100.0/num_vis) .gt. clip_limit) then
              do i = 1, num_vis
                  vis( 1, i ) = 0.0
                  vis( 2, i ) = 0.0
              end do
          end if

      else if ( int_chop_type .eq. 3 ) then
C         Initialise statistics variables
          sum_cos = 0.0D+0
          sum_sin = 0.0D+0
          ssq_cos = 0.0D+0
          ssq_sin = 0.0D+0
          n       = 0

          max_amp_sqr = max_signal*max_signal
          do i = 1, num_vis
              cos_sqr = vis(1,i)*vis(1,i)
              sin_sqr = vis(2,i)*vis(2,i)
              if ((cos_sqr+sin_sqr) .gt. max_amp_sqr ) then
                  vis(1,i) = 0.0
                  vis(2,i) = 0.0
              else if ((cos_sqr+sin_sqr).gt.0.0) then
                  sum_cos = sum_cos + vis(1,i)
                  sum_sin = sum_sin + vis(2,i)
                  ssq_cos = ssq_cos + cos_sqr
                  ssq_sin = ssq_sin + sin_sqr
                  n       = n + 1
              end if
          end do

C         Throw away whole buffer if either channel has a large variance
          if (n .gt. 0) then
              var_cos   = (ssq_cos - sum_cos*sum_cos/n)/n
              var_sin   = (ssq_sin - sum_sin*sum_sin/n)/n
              var_limit = max_noise*max_noise
              if (var_cos.gt.var_limit .or. var_sin.gt.var_limit) then
                  do i = 1, num_vis
                      vis(1,i) = 0.0
                      vis(2,i) = 0.0
                  end do
              end if
          end if

      else if ( int_chop_type .eq. 4 ) then
          do i = 1, num_vis
              if ( abs( vis( 1, i ) ) .gt. max_int_signal .or.
     *             abs( vis( 2, i ) ) .gt. max_int_signal      ) then
                  vis( 1, i ) = 0.0
                  vis( 2, i ) = 0.0
              end if
          end do

      else if ( int_chop_type .eq. 5 ) then
          j = 1
          level1 = amp_level(pre_post_index,1)
          level2 = amp_level(pre_post_index,1)
          spacing1 = 0
          spacing2 = sp_index(pre_post_index,1)
          do i = 1, num_vis
              if ( i .gt. sp_index(pre_post_index,j) )  then
                  if ( j .lt. npair(pre_post_index) ) then
                      j = j + 1
                      level1 = level2
                      level2 = amp_level(pre_post_index,j)
                      spacing1 = spacing2
                      spacing2 = sp_index(pre_post_index,j)
                  else
                      level1 = level2
                      spacing1 = num_vis + 1
                  endif
              endif
              level = level1
     *             + ((i-spacing1)*(level2-level1))/(spacing2-spacing1)
              if(vis(1,i)*vis(1,i)+vis(2,i)*vis(2,i).gt.level*level)then
                  vis(1,i) = 0.
                  vis(2,i) = 0.
              endif
          end do

      else if ( int_chop_type .eq. 6 ) then
C         Initialise statistics variables
          sum_cos = 0.0D+0
          sum_sin = 0.0D+0
          ssq_cos = 0.0D+0
          ssq_sin = 0.0D+0
          n       = 0

          max_amp_sqr = max_signal*max_signal
          do i = 1, num_vis
              cos_sqr = vis(1,i)*vis(1,i)
              sin_sqr = vis(2,i)*vis(2,i)
              if ((cos_sqr+sin_sqr) .gt. max_amp_sqr ) then
                  vis(1,i) = 0.0
                  vis(2,i) = 0.0
              else if ((cos_sqr+sin_sqr).gt.0.0) then
                  sum_cos = sum_cos + vis(1,i)
                  sum_sin = sum_sin + vis(2,i)
                  ssq_cos = ssq_cos + cos_sqr
                  ssq_sin = ssq_sin + sin_sqr
                  n       = n + 1
              end if
          end do

C         Throw away whole buffer if either channel has a large variance
          if (n .gt. 0) then
              var_cos   = (ssq_cos - sum_cos*sum_cos/n)/n
              var_sin   = (ssq_sin - sum_sin*sum_sin/n)/n
              var_limit = max_noise*max_noise
              if ( (var_cos + var_sin) .gt. var_limit) then
                  do i = 1, num_vis
                      vis(1,i) = 0.0
                      vis(2,i) = 0.0
                  end do
              end if
          end if


      else
          s = ILL_INTCHOP

      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine INT_CHOP' )
          return
      end
