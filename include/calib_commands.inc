C
C Commands for the calibration system section of POSTMORTEM
C
          integer         num_cmds, num_CLFST_cmds
          parameter      (num_cmds = 26)
          parameter      (num_CLFST_cmds = 14)

          character*70    cmd_list(num_cmds)

          integer         exit_cmd
          parameter      (exit_cmd = 0)

          integer         set_sample_file_cmd
          parameter      (set_sample_file_cmd = 1)
          data            cmd_list(set_sample_file_cmd)   /
     *'set-sample-file .... initialises a calibration for another file'/

          integer         set_lsf_cmd
          parameter      (set_lsf_cmd = 2)
          data            cmd_list(set_lsf_cmd)
     */'set-lsf ............ initialises a calibration for another LSF'/

          integer         open_cal_cmd
          parameter      (open_cal_cmd = 3)
          data            cmd_list(open_cal_cmd)
     * /'open-calibration .... selects an existing calibration to open'/

          integer         save_cal_cmd
          parameter      (save_cal_cmd = 4)
          data            cmd_list(save_cal_cmd)
     * / 'save-calibration .... saves the calibration to be made later'/

          integer         select_calib_cmd
          parameter      (select_calib_cmd = 5)
          data            cmd_list(select_calib_cmd)
     * / 'calibrator-file ..... set the sample file of the calibrator'/

          integer         display_cal_cmd
          parameter      (display_cal_cmd = 6)
          data            cmd_list(display_cal_cmd)
     * / 'print-calibration ... prints the lsf for the calibration'/

          integer         make_cal_cmd
          parameter      (make_cal_cmd = 7)
          data            cmd_list(make_cal_cmd)
     * / 'make-calibration .... makes all outstanding calibrations' /

          integer         interactive_cal_cmd
          parameter      (interactive_cal_cmd = 8)
          data            cmd_list(interactive_cal_cmd)
     * / 'go-calibration ...... makes outstanding calibrations on-line'/

          integer         display_sample_cmd
          parameter      (display_sample_cmd = 9)
          data            cmd_list(display_sample_cmd)
     * / 'display-sample ...... displays samples of the source model'/

          integer         display_spacing_cmd
          parameter      (display_spacing_cmd = 10)
          data            cmd_list(display_spacing_cmd)
     * / 'display-spacing ..... displays spacings of the source model'/

          integer         select_model_cmd
          parameter      (select_model_cmd = 11)
          data            cmd_list(select_model_cmd)
     * / 'model-source ........ defines the source model'/

          integer         select_amp_cmd
          parameter      (select_amp_cmd = 12)
          data            cmd_list(select_amp_cmd)
     * / 'amplitude-solution .. turns amplitude solution on/off'/

          integer         select_phase_cmd
          parameter      (select_phase_cmd = 13)
          data            cmd_list(select_phase_cmd)
     * / 'phase-solution ...... turns phase solution on/off'/

          integer         select_bandwidth_cmd
          parameter      (select_bandwidth_cmd = 14)
          data            cmd_list(select_bandwidth_cmd)
     * / 'bandwidth ........... sets the telescope bandwidth'/

          integer         select_refant_cmd
          parameter      (select_refant_cmd = 15)
          data            cmd_list(select_refant_cmd)
     * / 'reference-antenna ... sets the reference antenna'/

          integer         soln_monitor_cmd
          parameter      (soln_monitor_cmd = 16)
          data            cmd_list(soln_monitor_cmd)
     * / 'solution-monitor .... enter the solution monitor sub-system'/

          integer         gt_write_cmd
          parameter      (gt_write_cmd = 17)
          data            cmd_list(gt_write_cmd)
     * / 'write-to-gt-file .... write record to the gains table file'/

          integer         gt_delete_cmd
          parameter      (gt_delete_cmd = 18)
          data            cmd_list(gt_delete_cmd)
     * / 'delete-gt-record .... delete record in the gains table file'/


          integer         gt_update_cmd
          parameter      (gt_update_cmd = 19)
          data            cmd_list(gt_update_cmd)
     * / 'update-gains-table .. update the current gains table'/

          integer         gt_display_cmd
          parameter      (gt_display_cmd = 20)
          data            cmd_list(gt_display_cmd)
     * / 'display-gains-table . display the current gains table'/

          integer         gtvis_display_cmd
          parameter      (gtvis_display_cmd = 21)
          data            cmd_list(gtvis_display_cmd)
     * / 'display-vis-gains ... display the current visibility gains'/


          integer         gt_examine_cmd
          parameter      (gt_examine_cmd = 22)
          data            cmd_list(gt_examine_cmd)
     * / 'examine-GT-file ..... examine the gains table file'/

          integer         gt_apply_cmd
          parameter      (gt_apply_cmd = 23)
          data            cmd_list(gt_apply_cmd)
     * / 'apply-gains-table ... apply gains correction to sample file'/

          integer         gt_vissol_cmd
          parameter      (gt_vissol_cmd = 24)
          data            cmd_list(gt_vissol_cmd)
     * / 'visibility-solution . choose visibility-based solutions'/

        integer         cal_new_geom_cmd
        parameter      (cal_new_geom_cmd = 25)
        data            cmd_list(cal_new_geom_cmd)
     * / 'new-geometry ....... create new cal with revised geom'/

        integer         gt_undelete_cmd
        parameter      (gt_undelete_cmd = 26)
        data            cmd_list(gt_undelete_cmd)
     * / 'undelete-gt-record .. recover deleted record'/


