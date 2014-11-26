C
C+ cal_gt_apply

       subroutine cal_gt_apply( psf_name, s )
C      --------------------------------------
C
C Apply the current gains correction table to the named physical SF
C
C      Given:
C          physical sample file
                  character*(*)    psf_name
C      Returned:
C          error status
                  integer          s
C
C The current gains table is applied to the physical sample file.  A
C calibration file is created for the named physical sample file if it
C does not already exist: a new 'source' is added to the new or existing
C gains table using the amplitude and phase corrections of the current
C gains table.
C
C PA 12/11/91
C-
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/src_pack.inc'
       include '/mrao/post/include/cal_common.inc'
       include '/mrao/post/include/cal_solution.inc'

C Local variables
C         Logical unit numbers: physical sample file, calibration file
              integer             sf_lun, cf_lun
C         Number of samples and number of visibilities in the SF
              integer             num_samp, num_vis
C         Source number in calibration file
              integer             src_num
C         Visibility list
              integer             vis_list(max_vis)
C         Calibration file name
              character*64        cal_file_name
C         Loop counter
              integer             iv
C         Gain corrections
              complex             gains(max_vis)
C         Index to sub-bands, channels, aerials and spacings
              integer             isb, ich, iae1, iae2, isp


C Subroutine initialisation
C -------------------------

C Check for non zero entry status
      if ( s .ne. 0 ) return

      if (.not.current_solution .and. .not.current_gains_table) then
        call io_wrout('*** Gains table not initialised')
        return
      end if


C Main Code
C ---------

C create a new source in the calibration file for the sample file which
C is being calibrated

C open the sample file to calibrate
       call open_sf( sf_lun, psf_name, 'READ', 0, s )
       call open_source( sf_lun, 1, 0, s )
       call enq_numsamp( sf_lun, 1, num_samp, s )
       call enq_numvis( sf_lun, num_vis, s )
C create the source in the calibration sample file
       call create_source( sf_lun, 'CAL',
     *                     1, num_samp,
     *                     1, num_vis,
     *                     src_num, s   )
       call close_source( sf_lun, 1, s )

C find the calibration file name
       call enq_namfil( sf_lun, 'CAL', cal_file_name, s )

C open the calibration sample file for write access
       call open_sf( cf_lun, cal_file_name, 'WRITE', 0, s )
       call set_src_def( cf_lun, src_num, cal_record, s )
       call close_sf( cf_lun, s )
       call open_sf( cf_lun, cal_file_name, 'WRITE', 0, s )
       call open_source( cf_lun, src_num, 0, s )

C determine the gains for each visibility
       do iv=1,num_vis
         vis_list(iv) = iv
         call enq_vis_desig( sf_lun, vis_list(iv), isp, isb, ich, s)
         call enq_ae_vis( sf_lun, vis_list(iv), iae1, iae2, s )
         gains(iv) = vis_gains(iv) *
     *        ae_gains(ich,isb,iae1)*conjg(ae_gains(ich,isb,iae2))
       end do

C write the gains to the calibration file
       call enq_src_pack( cf_lun, src_num, src_pack, s )
       call write_rt( cf_lun, src_num, 1,
     *                cal_ra, cal_dec, src_stop_time, s )
       call write_sample( cf_lun, src_num, 1, 1,
     *                    num_vis, vis_list, gains, s )

C update the calibration record
       call util_enqnow( cal_key )
       call io_getstr( 'Calibration name : ', '*', cal_source, s )
       call set_src_def( cf_lun, src_num, cal_record, s )

C close the calibration sample file
       call close_sf( cf_lun, s )

C close the sample file which is being calibrated
       call close_sf( sf_lun, s )
c      call save_sf( cal_file_name, s )

       if (s.ne.0) call cal_wrerr( s, 'in subroutine cal_gt_apply' )
       end
