
C     *****************************************************************
C
C+lsf_get_calgain
C
      SUBROUTINE lsf_get_calgain( lsf_num,
     *                            max_num_vis,
     *                            gains,
     *                            num_vis,
     *                            s                      )

C
C     Gets the current calibration gains buffer
C
C     Given:
C         Logical sample file number.
              integer             lsf_num
C         Maximum size of the visibility buffer.
              integer             max_num_vis
C
C     Returned:
C         Gains buffer
              complex             gains( max_num_vis )
C         Number of visibility gains in the buffer.
              integer             num_vis
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The routine returns the calibration gains for the current LSF
C     buffer.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/samplib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         <description>

C     Variables, equivalences and commons
C         Loop counter.
              integer         i
C         The actual sample currently being accessed.
              integer         samp_num
C         Smoothing count - number of samples smoothed so far.
              integer         smooth_count

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      if ( sp_list_len .gt. max_num_vis ) then
          s = ILL_BUFFSIZE
          goto 9999
      else
          num_vis = sp_list_len
      end if

      smooth_count = 0
      samp_num     = curr_samp

C     ****************************************************************
C
C         Main Code
C         ---------
C

  100 continue
          call read_rt( sf_lun,
     *                  src_num,
     *                  samp_num,
     *                  raw_samp_ra, raw_samp_dec,
     *                  samp_sid,
     *                  s               )
          do i = 1,num_vis
            gains(i) = cmplx(1.0,0.0)
          end do

          if (s .eq. 0) then
C             Do the processing.
              call calibrate( cf_lun,
     *                        lsf_cal_num,
     *                        sp_list_len,
     *                        gains,
     *                        sp_list,
     *                        samp_sid,
     *                        .true.,
     *                        s               )

          end if
          call smooth(    sp_list_len,
     *                    gains,
     *                    smooth_type,
     *                    smooth_size,
     *                    smooth_count,
     *                    s                   )
          if ( s .ne. 0 ) goto 9999


          if ( smooth_count .ne. 0 ) then
              samp_num = curr_samp+smooth_count
              call read_rt( sf_lun,
     *                      src_num,
     *                      samp_num,
     *                      raw_samp_ra, raw_samp_dec,
     *                      samp_sid,
     *                      s               )
              samp_mjd=mjd_st0+dble(samp_sid)/(864000.D+0*const_sut2sst)
              if (planet_flg) then
                  call planet_topo( source_ucase, samp_mjd,
     *                              epoch_ra, epoch_dec, s    )
              end if
              if ( s .ne. 0 ) goto 9999
          end if
      if ( smooth_count .ne. 0 ) goto 100

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          print *, 'Sample, buffer = ', curr_samp, curr_buff
          call lsf_wrerr( s, 'in subroutine LSF_GET_CALGAIN' )
          return
      end
