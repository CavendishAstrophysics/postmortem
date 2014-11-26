C+lsf_get_model
C
      SUBROUTINE lsf_get_model( lsf_num,
     *                          max_num_vis,
     *                          vis,
     *                          num_vis,
     *                          s                      )

C
C     Gets the the model visibilities for the current visibility buffer.
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
C     The routine that does all the work for obtaining model
C     visibilities. If no model is currently defined then the default
C     model visibilities are returned (point source of unit height at
C     phase centre).
C
C     Modified by PJW to include effects of integration time smearing
C                                                          ( 8/91 )
C         and to allow for model apertures whose 'map-centres' are
C         different from current one.                      (10/91 )
C
C         and again moving the call to integ_time outside the smooth
C         loop. So user must provide effective integration time.  (3/92)
C
C         no the above was wrong because it was moved outside the
C         multiple source loop                                    (7/92)
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/lsf_definition.inc'
      include '/mrao/post/include/lsf_runtime.inc'
      include '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         <description>

C     Variables, equivalences and commons
C         Loop counter.
              integer         i, j
C         The actual sample currently being accessed.
              integer         samp_num
C         Smoothing count - number of samples smoothed so far.
              integer         smooth_count
C         Model visibilities for a given source
              complex         source_vis( max_rt_sp )
C         Apparent source flux after primary beam correction.
              real            app_flux
C         Offset to convert ra's to hour angles
              real*8          ha_offset
C         'Apparent' source RA after adding ha_offset.
              real*8          app_ra
C         Actual ra and dec of data in visibility buffer
              real*8          current_ra, current_dec
C         Integration time (10ths siderial secs)
C             integer         integration_time
C         Appropriate positions for bandwidth and integration time
C             smearing
              real*8          pc_ra, pc_dec, obs_ra, obs_dec
C         Unwanted sample file details
              character*24    name
              real*8          sf_epoch
C         Physical sample file logical unit no. and source no.
              integer         sf_unit, src_no

C  *********************************************************************
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

      if (mod_num_srcs .le. 0) then
          mod_ra(1)   = epoch_ra
          mod_dec(1)  = epoch_dec
          mod_flux(1) = 1.0
          mod_type(1) = 1
      end if

C      call lsf_enq_integ(lsf_num, integration_time, s )

      call lsf_enq_sf(lsf_num, sf_unit, src_no, s )
      call enq_path_comp( sf_unit, pc_ra, pc_dec, s )
      call enq_pc_epoch( sf_unit, src_no,
     *                   sf_epoch, obs_ra, obs_dec, name, s )

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
          do 200, i = 1, sp_list_len
              vis( i ) = (0.0, 0.0)
  200     continue

          if ( mod_ra_aes .lt. 0.0D+0 ) then
C             Aerials are fixed - find RA to HA conversion offset.
              ha_offset = dble(samp_sid)*const_st2r/10.0D+0 +baseln_skew
          else
              ha_offset = 0.0D+0
          end if

          do 500, i = 1, max( mod_num_srcs, 1 )
C             Set up the model fluxes.
              app_ra = mod_ra(i) - ha_offset
              if (mod_pb_flg) then
                  call pbcorr( app_ra, mod_dec(i),
     *                         mod_ra_aes, mod_dec_aes,
     *                         mod_tscope, app_flux, s       )
                  app_flux = mod_flux(i)*app_flux
              else
                  app_flux = mod_flux(i)
              end if

              current_ra = mod_ra(i)
              current_dec = mod_dec(i)
              call model_vis( mod_type(i), app_flux,
     *                        current_ra, current_dec, source_vis, s )

C             Apply bandwidth smearing
              call bandwidth( sp_list_len,
     *                        source_vis,
     *                        base,
     *                        pc_ra, pc_dec,
     *                        current_ra, current_dec,
     *                        samp_sid, baseln_skew,
     *                        mod_band_type,
     *                        mod_band_width,
     *                        s           )

C             Apply integration-time smearing
              call integ_time( sp_list_len,
     *                        source_vis,
     *                        base,
     *                        obs_ra, obs_dec,
     *                        current_ra, current_dec,
     *                        samp_sid, baseln_skew,
     *                        mod_integ_time,
     *                        s           )


C             Phase shift to phase centre.
              call phase_rot( sp_list_len,
     *                        source_vis,
     *                        base,
     *                        current_ra,  current_dec,
     *                        epoch_ra, epoch_dec,
     *                        samp_sid, baseln_skew,
     *                        0.0,
     *                        s                 )

C             Add in source visibilities
              do 400, j = 1, sp_list_len
                  vis( j ) = vis( j ) + source_vis( j )
  400         continue
  500     continue
          raw_samp_ra  = epoch_ra
          raw_samp_dec = epoch_dec

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

C     Apply integration-time smearing
C          this is the correct place since the effect is non-linear
C     NO! this does NOT get multiple source models correct 23/7/92

C      call integ_time( sp_list_len,
C     *                 vis,
C     *                 base,
C     *                 obs_ra, obs_dec,
C     *                 current_ra, current_dec,
C     *                 samp_sid, baseln_skew,
C     *                 mod_integ_time,
C     *                 s           )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          print *, 'Sample, buffer = ', curr_samp, curr_buff
          call lsf_wrerr( s, 'in subroutine LSF_GET_MODEL' )
          return
      end

C     *****************************************************************
C

