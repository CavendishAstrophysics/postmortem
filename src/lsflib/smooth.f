
C     *****************************************************************
C
C+smooth
C
      SUBROUTINE smooth(  num_vis,
     *                    vis,
     *                    type,
     *                    size,
     *                    smooth_count,
     *                    s                      )

C
C     Applies smoothing to a visibility buffer
C
C     Given:
C         Number of spacings.
              integer         num_vis
C         Visibility buffer
              complex         vis( num_vis )
C         Smoothing type.
              integer         type
C         Smoothing size.
              integer         size

C     Returned:
C         Smooth count - increment to the current sample for the next
C         sample to be got for smoothing. (see comments below)
              integer         smooth_count
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Applies smoothing of various sorts to the visibility buffer.
C
C     This routine is basically controlled by use of the smooth_count
C     variable.
C
C     If this is zero on input then smoothing is initialised using the
C     current visibility buffer.
C
C     On return its value is the offset from the current sample to
C     the next sample to be got for smoothing. The sample indicated
C     will be in the logical sample file and within the confines of
C     the physical sample file.
C
C     When an offset of zero is returned the buffer contains valid,
C     smoothed data and the sample redtape values samp_sid, raw_samp_ra,
C     and raw_samp_dec in the lsf-realtime common blocks contain the
C     correct values for the smoothed data. Otherwise these values and
C     the buffer are unchanged.
C
C     An internal buffer overflow is indicated by an ILL_BUFFSIZE
C     status.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      logical         util_tstbit

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Constant declarations
C         Internal buffer size
              integer         buff_size
              parameter     ( buff_size = max_rt_sp )

C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Final sample in current smooth operation
              integer         end_buff
C         Current sample in current smooth operation
              integer         smooth_samp
C         Null sample buffer flag - ie: all visibilities zero.
              logical         null_sample
C         End of smoothing indicator
              logical         end_smooth
C         Total number of samples smoothed
              integer         sample_count
C         Temporary internal accumulation buffers.
              complex*16      buffer( buff_size )
              real*8          count( buff_size )
C         Redtape sums for averaging redtape
              real*8          sum_sid, sum_ra, sum_dec
C         Ionispheric correction buffer
              real            ion(4)
C         Visibility weighting factors
              complex         scale_fact
              real            weight_fact

          common / smooth_1 / buffer, count, sum_sid, sum_ra, sum_dec,
     *                        scale_fact, weight_fact, sample_count

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( num_vis .gt. buff_size ) then
          s = ILL_BUFFSIZE
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C

      if ((type .eq. 0) .or. (size .le. 1)) then
          continue
      else if (type .ge. 1 .and. type .le. 3 ) then
          smooth_samp = curr_samp  + smooth_count
          end_buff    = min( (start_buff+size-1), num_samp )

          if (type.eq.1.or.lsf_ion_num.le.0) then
              scale_fact  = (1.0,0.0)
              weight_fact = 1.0
          else
              call read_ion_corr( sf_lun,
     *                            lsf_ion_num,
     *                            smooth_samp,
     *                            ion,
     *                            s               )

              if (s.eq.NO_IONCORR) then
                  s = 0
                  scale_fact = (0.0,0.0)
                  weight_fact= 0.0
              else if (s.ne.0) then
                  goto 9999
              else if (type .eq. 2) then
                  if (ion(3) .ne. 0) then
                      scale_fact  = cmplx(1.0/ion(3),0.0)
                  else
                      scale_fact = (0.0,0.0)
                  end if
                  weight_fact = 0.0
              else
                  scale_fact  = cmplx(ion(3),0.0)
                  weight_fact = ion(3)*ion(3)
              end if
          end if

          null_sample = .true.
          if (smooth_samp.lt.curr_samp.or.smooth_samp.gt.end_buff) then
C             Current smooth sample is out of permitted range
              s = ILL_SMOOTH
              goto 9999
          else if (smooth_samp.eq.curr_samp) then
C             Start of smoothing - initialise averaging arrays
              do 100, i = 1, num_vis
                  buffer( i ) = vis( i )*scale_fact
                  if ( vis( i ) .eq. ( 0.0, 0.0 ) ) then
                      count( i ) = 0.0
                  else
                      count( i ) = weight_fact
                      null_sample = .false.
                  end if
  100         continue
              if (null_sample) then
                  sample_count = 0
                  sum_sid      = 0.0D+0
                  sum_ra       = 0.0D+0
                  sum_dec      = 0.0D+0
              else
                  sample_count = 1
                  sum_sid      = dble(samp_sid)
                  sum_ra       = raw_samp_ra
                  sum_dec      = raw_samp_dec
              end if
          else
C             Continue averaging
              do 200, i = 1, num_vis
                  if ( vis( i ) .ne. ( 0.0, 0.0 ) ) then
                      buffer( i ) = buffer( i ) + (vis( i )*scale_fact)
                      count( i )  = count( i ) + weight_fact
                      null_sample = .false.
                  end if
  200         continue
              if ( .not. null_sample ) then
                  sample_count = sample_count + 1
                  sum_sid = sum_sid + dble(samp_sid)
                  sum_ra  = sum_ra  + raw_samp_ra
                  sum_dec = sum_dec + raw_samp_dec
              end if
          end if

C         Find next sample for smoothing.
  300     continue
              smooth_samp = smooth_samp + 1
              end_smooth  = (smooth_samp .gt. end_buff)
          if (.not.(util_tstbit(samp_list,smooth_samp) .or. end_smooth))
     *                                                         goto 300

          if ( end_smooth ) then
              smooth_count = 0
              do 400, i = 1, num_vis
                  if ( count( i ) .ne. 0 ) then
                      vis( i ) = buffer( i )/cmplx(count( i ),0.0)
                  else
                      vis( i ) = ( 0.0, 0.0 )
                  end if
  400         continue
              if (sample_count .ne. 0) then
                  samp_sid     = nint(sum_sid/sample_count)
                  epoch_ra     = sum_ra  / dble(sample_count)
                  epoch_dec    = sum_dec / dble(sample_count)
                  raw_samp_ra  = epoch_ra
                  raw_samp_dec = epoch_dec
              end if
          else
              smooth_count = smooth_samp-curr_samp
          end if
      else
          s = ILL_SMOOTH
      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine SMOOTH' )
          return
      end
