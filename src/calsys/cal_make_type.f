C+cal_make_type
C
      SUBROUTINE cal_make_type( cf_lun, src_num, s )

C     Calculates the current calibration in the cal record
C
C     Given:
C         Calibration file unit number - must be open for write.
              integer             cf_lun
C         Calibration source number - should be closed, closed on exit.
              integer             src_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/src_pack.inc'
      include  '/mrao/post/include/calib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'
      include  '/mrao/post/include/global_constants.inc'

C
C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i, ic
C         Logical sample file spacing list
              integer         sp_list(max_vis)
C         Spacing list for previous calibration
              integer         cal_list(max_vis)
C         List of visibilities returned from get_vis_buffer
              complex         vis_list(max_vis)
C         List of model visibilities returned from cal_def_model
              complex         mod_vis(max_vis)
C         List of derived spacing calibrations
              complex         gains(max_vis)
C         List of previous spacing calibrations
              complex         prev_gains(max_vis)
C         Incremental spacing calibration buffer
              complex         gains_buff(max_vis)
C         Number of visibilities in the buffer
              integer         num_vis
C         Logical sample file number and its sample file unit number
              integer         lsf_num, sf_lun
C         Number of visibilities in the previous calibration buffer
              integer         cal_nvis
C         Sample file unit number and source number for previous cal.
              integer         cal_lun, cal_src
C         Sample count and total samples for previous calibration
              integer         cal_samp, cal_nsamp
C         Number of buffers and current buffer of LSF
              integer         num_buff, samp_num, buff_num
C         Single sample redtape.
              integer         sid, cal_sid
              real*8          ra, dec
C         Final sample in the current LSF buffer
              integer         last_samp

C         Workspace
              common /post/ sp_list, cal_list, vis_list,
     *                      mod_vis, gains, prev_gains, gains_buff

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C Check for non zero entry status
      if (s .ne. 0) return

C Initialise buffers
      do i = 1, max_spac
         gains(i) = cmplx(1.0,0.0)
         prev_gains(i) = cmplx(1.0,0.0)
         gains_buff(i) = cmplx(1.0,0.0)
      end do

C Open the calibration source
      call open_source(cf_lun, src_num, 0, s)

C
C     Main Code
C     ---------
C
C Open the LSF of the calibration and initialise
      call cal_open_lsf('READ', lsf_num, s)
      call lsf_enq_sf(lsf_num, sf_lun, i, s)
      call lsf_enq_numbuff(lsf_num, num_buff, s)
      call lsf_set_pc(lsf_num, cal_refdat,
     *                 cal_ra(1), cal_dec(1), cal_source, s)
      call lsf_set_spacings(lsf_num, num_vis, sp_list, 3, s)
      if (s .ne. 0) goto 9999

C Find details of any previous calibration from the LSF definition
      call lsf_enq_cf(lsf_num, cal_lun, cal_src, s)
      if (cal_lun .gt.0) then
         call enq_numvis(cal_lun, cal_nvis, s)
         call enq_numsamp(cal_lun, cal_src, cal_nsamp, s)
         do i = 1, cal_nvis
           cal_list(i) = i
         end do
         cal_samp = 1
      else
         cal_nvis = num_vis
         do i = 1, cal_nvis
           cal_list(i) = sp_list(i)
         end do
      end if

C Loop through and for each sample in the calibration LSF determine
C the new gains, writing the results to the calibration file

      samp_num = 0
      buff_num = 1
 100  call lsf_set_buffer(lsf_num, buff_num, s)

C .. find ST of last sample of this buffer
         call lsf_enq_samples(lsf_num, buff_num, i, last_samp, s)
         call read_rt(sf_lun, 1, last_samp, ra, dec, sid, s)

C .. set up buffer containing previous calibration gains
         if (cal_lun .gt. 0) then
            call read_sample(cal_lun, cal_src, cal_samp, 1,
     *                              cal_nvis, cal_list, prev_gains, s)
 101        if (cal_samp .lt. cal_nsamp) then
C .. compare ST and skip to insert an extra output sample if necessary
               call read_rt(cal_lun, cal_src, cal_samp+1, ra, dec,
     *                                                     cal_sid, s)
               if (sid .gt. cal_sid) then
                  cal_samp = cal_samp + 1
C .. do not insert extra output samples (DJT, 15/3/93)
c                 sid = cal_sid
c                 goto 200
                  goto 101
               end if
            end if
         end if

C .. get visibilities and model visibilities for this buffer
         call lsf_get_vis(lsf_num, max_vis, vis_list, num_vis, s)
         call lsf_get_model(lsf_num, max_vis, mod_vis, num_vis, s)

C .. get mean ST for this buffer (DJT, 24/11/92)
         if (cal_type.eq.2 .or. cal_type.eq.3) then
            call lsf_get_sid(lsf_num, sid, s)
         end if

C .. use the visibilities and model visibilities to solve for the new
C    spacing gains (type-1 or type-2 method)
         if (cal_type.eq.1) then
            call cal_fact_type1(cf_lun,
     *                           num_vis,
     *                           sp_list,
     *                           vis_list,
     *                           mod_vis,
     *                           cal_merge_type,
     *                           gains,
     *                           s              )
         else if (cal_type.eq.2) then
            call cal_fact_type2(cf_lun,
     *                           num_vis,
     *                           sp_list,
     *                           vis_list,
     *                           mod_vis,
     *                           cal_merge_type,
     *                           gains,
     *                           s              )
         else if (cal_type.eq.3) then
            call cal_fact_type3(cf_lun,
     *                           num_vis,
     *                           sp_list,
     *                           vis_list,
     *                           mod_vis,
     *                           cal_merge_type,
     *                           gains,
     *                           s              )
         else
            s = ILL_CALIBRATION
            goto 9999
         end if

C .. modify these gains depending on the type of solution required
         if (cal_no_amp) then
            if (cal_type.eq.2 .or. cal_type.eq.3) then
              call cal_gt_noamp(s)
            end if
            do i = 1, num_vis
               if (gains(i).ne.(0.0,0.0)) then
                  gains(i) = gains(i)/cmplx(cabs(gains(i)),0.0)
               end if
            end do
         end if

         if (cal_no_phi) then
            if (cal_type.eq.2 .or. cal_type.eq.3) then
              call cal_gt_nophi(s)
            end if
            do i = 1, num_vis
               gains(i) = cmplx(cabs(gains(i)),0.0)
            end do
         end if

         buff_num = buff_num + 1

 200     samp_num = samp_num + 1

C .. combine new gains with gains from any previous calibration
         i = 1
         do ic = 1, cal_nvis
           gains_buff(ic) = prev_gains(ic)
           if (cal_list(ic) .eq. sp_list(i)) then
             gains_buff(ic) = prev_gains(ic)*gains(i)
             if (i .lt. num_vis) i = i + 1
           end if
         end do

C .. write sample redtape and data to the calibration file
         call write_rt(cf_lun, src_num, samp_num, cal_ra(1), cal_dec(1),
     *                                                          sid, s)
         call write_sample(cf_lun, src_num, samp_num, 1,
     *                               cal_nvis, cal_list, gains_buff, s)

      if (buff_num .le. num_buff .and. s.eq.0) goto 100


C Close the calibration LSF
      call lsf_close(lsf_num, s)

C Update and close the calibration file
      call enq_src_pack(cf_lun, src_num, src_pack, s)
      src_num_samp = samp_num
      if (src_interp_type.eq.1) then
         call write_rt(cf_lun, src_num, src_num_samp,
     *                  cal_ra(1), cal_dec(1), src_stop_time, s)
      end if
      call set_src_pack(cf_lun, src_num, src_pack, s)
      call close_source(cf_lun, src_num, s)
      if (s .ne. 0) goto 9999

      call util_enqnow(cal_key)
      call set_src_def(cf_lun, src_num, cal_record, s)
      if (s .ne. 0) goto 9999

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr(s, 'in subroutine cal_make_type ')
          return
      end

C
C
