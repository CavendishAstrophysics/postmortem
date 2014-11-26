C+lsf_get_vis
C
      SUBROUTINE lsf_get_vis( lsf_num,
     *                        max_num_vis,
     *                        vis,
     *                        num_vis,
     *                        s                      )

C
C     Gets the current visibility buffer.
C
C     Given:
C         Logical sample file number.
              integer             lsf_num
C         Maximum size of the visibility buffer.
              integer             max_num_vis
C
C     Returned:
C         Visibility buffer
              complex             vis( max_num_vis )
C         Number of visibilities in the buffer.
              integer             num_vis
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The routine that does all the work for obtaining a logical sample.
C     Takes a raw sample and does all the processing specified by the
C     logical sample file description.
C
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
C         Current ionospheric correction information buffer
              real            ion( 4 )
C         Telescope name and id number
              character*80    tscope_name
              integer         tscope

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

      call enq_tscope(sf_lun, tscope_name, tscope, s)


C     ****************************************************************
C
C         Main Code
C         ---------
C
  100 continue
          call read_sample(   sf_lun,
     *                        src_num,
     *                        samp_num,
     *                        1,
     *                        sp_list_len,
     *                        sp_list,
     *                        vis,
     *                        s               )

          if ( s .eq. BAD_SAMPLE ) then
C             Then the buffer is all null and no processing is done.
              s = 0
              do 200, i = 1, sp_list_len
                  vis(i) = (0.0, 0.0)
  200         continue
              raw_samp_ra  = epoch_ra
              raw_samp_dec = epoch_dec
          else if (s .eq. 0) then
C             Do all the processing.
              call flagging(  lsf_num,
     *                        sf_lun,
     *                        samp_num,
     *                        sp_list_len,
     *                        sp_list,
     *                        vis,
     *                        s               )
              call int_chop(  sp_list_len,
     *                        vis,
     *                        pre_int_chop_type,
     *                        pre_int_chop_params,
     *                        1,
     *                        s                   )

C             Do removes which have a different calibration to the LSF
              do 300, i = 1, num_removes
                  if (lsf_rem_cal(i).ne.lsf_cal_num) then
                      call remove_source( rf_lun, lsf_rem_num(i),
     *                                    lsf_rem_type(i),
     *                                    cf_lun, lsf_rem_cal(i),
     *                                    sf_lun, lsf_rem_ion(i),
     *                                    samp_num,
     *                                    sp_list_len,
     *                                    vis,
     *                                    raw_samp_ra, raw_samp_dec,
     *                                    samp_sid,
     *                                    sp_list,
     *                                    base, baseln_skew,
     *                                    s                   )
                  end if
  300         continue

C             Fudge amplitudes and fiddle phases
              call calibrate( cf_lun,
     *                        lsf_cal_num,
     *                        sp_list_len,
     *                        vis,
     *                        sp_list,
     *                        samp_sid,
     *                        .true.,
     *                        s               )

C             Do removes which have the same calibration as the LSF
              do 400, i = 1, num_removes
                  if (lsf_rem_cal(i).eq.lsf_cal_num) then
                      call remove_source( rf_lun, lsf_rem_num(i),
     *                                    lsf_rem_type(i),
     *                                    0, 0,
     *                                    sf_lun, lsf_rem_ion(i),
     *                                    samp_num,
     *                                    sp_list_len,
     *                                    vis,
     *                                    raw_samp_ra, raw_samp_dec,
     *                                    samp_sid,
     *                                    sp_list,
     *                                    base, baseln_skew,
     *                                    s                   )
                  end if
  400         continue

C             Get ionospheric correction for the LSF phase centre.
              if ( lsf_ion_num .ne. 0 ) then
                  call read_ion_corr( sf_lun, lsf_ion_num,
     *                                samp_num, ion, s )
              else
                  ion( 1 ) = 0.0
              end if

C             Phase shift and apply ionospheric correction (if allowed).
              if (sf_type .ne. 3) then
                  call phase_rot( sp_list_len,
     *                            vis,
     *                            base,
     *                            raw_samp_ra,  raw_samp_dec,
     *                            epoch_ra, epoch_dec,
     *                            samp_sid, baseln_skew,
     *                            ion(1),
     *                            s                 )
                  raw_samp_ra  = epoch_ra
                  raw_samp_dec = epoch_dec
              end if

C             Post-processing chop here for clfst
              if (tscope.eq.1) then
                    call int_chop(  sp_list_len,
     *                            vis,
     *                            post_int_chop_type,
     *                            post_int_chop_params,
     *                            2,
     *                            s                   )
              end if

          else
              call io_wrout( 'Error occured in read sample' )
          end if

          call smooth(    sp_list_len,
     *                    vis,
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

C      Post-processing chop here for Ryle etc
       if (tscope.ne.1) then
             call int_chop(  sp_list_len,
     *                      vis,
     *                      post_int_chop_type,
     *                      post_int_chop_params,
     *                      2,
     *                      s                   )
       end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          print *, 'Sample, buffer = ', curr_samp, curr_buff
          call lsf_wrerr( s, 'in subroutine LSF_GET_VIS' )
          return
      end

C     *****************************************************************
C
