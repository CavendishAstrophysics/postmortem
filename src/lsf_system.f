C
C+lsf_system
C
      subroutine lsf_system (def_sf, def_lsf, s)

C
C     The logical sample file system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current logical sample file key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Combines all the logical sample files functions in one place.
C
C last mod 10 July 2003  GP
C-
C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/include/iolib_functions.inc'
          include '/mrao/include/chrlib_functions.inc'
          include '/mrao/post/include/post_common.inc'
          include '/mrao/post/include/lsf_commands.inc'
          include '/mrao/post/include/phys_tscopes.inc'
          include '/mrao/post/include/lsflib_errors.inc'
          include '/mrao/post/include/samplib_errors.inc'
          include '/mrao/post/include/int_chop_record.inc'

C     Variables, equivalences and commons
C         Current command.
              integer         command
C         New sample file name
              character*80    new_sf
C         Current lsf number and new lsf key.
              integer         lsf_num, new_lsf
C         Interference chop type
              integer         int_chop_type
C         Command line prompt and length
              character*40    prompt
              integer         lp
              data            prompt, lp / 'LSF> ', 5 /
C         General purpose string and length
              character*80    string
              integer         ls
C         Record number of last lsf written to LSF file
              integer         rec_num
C         Telescope code
              integer         tscope
C         Current and previous output devices, terminal unit nos
              integer         out, old_out, termi, termo
C         Physical sample file unit number and source number
              integer         sf_lun, src_num

C     ****************************************************************
C
C     Subroutine initialisation
C
      if (s .ne. 0) return

      call io_wrout (' ')
      call io_wrout (def_sf)
      call io_wrout (' ')

      call lsf_open (def_sf, def_lsf, 'READ', lsf_num, s)
      call lsf_enq_sf (lsf_num, sf_lun, src_num, s)
      call enq_phys_tscope (sf_lun, tscope, s)
      if (s .ne. 0) goto 9999

C
C Main Code
C ---------
C

  100 continue
        call cmd_getcmd (prompt (1:lp), cmd_list, num_cmds, command, s)

        if (s .ne. 0) then
            continue

        else if (command .le. 0) then
            call basic_commands (command, prompt, lp, s)

        else if (command .eq. set_sample_file_cmd) then
            call lsf_close (lsf_num, s)
            new_sf  = ' '
            new_lsf = -1
            call open_sf (sf_lun, new_sf, 'READ', 0, s)
            if (s .eq. 0) then
                call close_sf (sf_lun, s)
                def_sf = new_sf
                def_lsf = -1
                call io_wrout (' ')
                call io_wrout (def_sf)
                call io_wrout (' ')
                call lsf_open (new_sf, new_lsf, 'READ', lsf_num, s)
            else if (s .eq. USR_BREAK) then
                s = 0
                call lsf_open (def_sf, def_lsf, 'READ', lsf_num, s)
            end if
            call lsf_enq_sf (lsf_num, sf_lun, src_num, s)
            call enq_phys_tscope (sf_lun, tscope, s)

        else if (command .eq. set_lsf_cmd) then
            call lsf_close (lsf_num, s)
            new_lsf = 0
            call lsf_open (def_sf, new_lsf, 'READ', lsf_num, s)
            if (s .eq. 0) then
                def_lsf = new_lsf
            else if (s .eq. NO_LSFSAVED) then
                s = 0
                call io_wrout ('No logical sample files are saved.')
                call lsf_open (def_sf, def_lsf, 'READ', lsf_num, s)
            else if (s .eq. USR_BREAK) then
                s = 0
                call lsf_open (def_sf, def_lsf, 'READ', lsf_num, s)
            else
                goto 9999
            end if
            call io_wrout (' ')

        else if (command .eq. lsf_save_cmd) then
            call lsf_save (lsf_num, def_lsf, s)
            call lsf_enlrec (rec_num, s)
            write (string (1:3),' (I3)') rec_num
            call cmd_setparam ('%LSF-number',string (1:3),s)

        else if (command .eq. list_lsf_cmd) then
            call lsf_list_def (sf_lun, s)

        else if (command .eq. delete_lsf_cmd) then
            call lsf_delete_def (sf_lun, 1, s)

        else if (command .eq. undelete_lsf_cmd) then
            call lsf_delete_def (sf_lun, 0, s)

        else if (command .eq. select_smooth_cmd) then
            call lsf_sel_smooth (lsf_num, s)

        else if (command .eq. select_average_cmd) then
            call lsf_sel_average (lsf_num, s)

        else if (command .eq. select_int_chop_cmd) then
            call lsf_enq_int_chop (lsf_num, int_chop_type,
     *                             int_chop_record, .true., s)
            if (io_yesno ('Apply pre-processing interference chop ?',
     *                                    'No', s)) then
                call lsf_sel_int_chop (int_chop_type,
     *                                 int_chop_record, s)
            else
                int_chop_type = 0
            end if
            call lsf_set_int_chop (lsf_num, int_chop_type,
     *                             int_chop_record, .true., s)

            call lsf_enq_int_chop (lsf_num, int_chop_type,
     *                             int_chop_record, .false., s)
            if (io_yesno ('Apply post-processing interference chop ?',
     *                                    'No', s)) then
                call lsf_sel_int_chop (int_chop_type,
     *                                 int_chop_record, s)
            else
                int_chop_type = 0
            end if
            call lsf_set_int_chop (lsf_num, int_chop_type,
     *                             int_chop_record, .false., s)

        else if (command .eq. select_ph_cent_cmd) then
            call lsf_sel_ph_cent (lsf_num, s)

        else if (command .eq. select_samples_cmd) then
            call lsf_sel_samples (lsf_num, s)

        else if (command .eq. add_samples_cmd) then
            call lsf_add_samples (lsf_num, s)

        else if (command .eq. select_calib_cmd) then
            call lsf_sel_cal (lsf_num, s)

        else if (command .eq. select_flagging_cmd) then
            call lsf_sel_flag (lsf_num, s)

        else if (command .eq. select_spacings_cmd) then
            call lsf_sel_spacings (lsf_num, s)

        else if (command .eq. select_notspac_cmd) then
            call lsf_sel_notspac (lsf_num, s)

        else if (command .eq. select_ion_corr_cmd) then
            call lsf_sel_ion_corr (lsf_num, s)

        else if (command .eq. select_removes_cmd) then
            call lsf_sel_removes (lsf_num, s)

        else if (command .eq. display_sample_cmd) then
            call display_sample (lsf_num, .true., plot_device, s)

        else if (command .eq. display_noise_cmd) then
            call display_noise (lsf_num, plot_device, s)

        else if (command .eq. display_hist_cmd) then
            call display_hist (lsf_num, plot_device, s)

        else if (command .eq. display_spacing_cmd) then
            call display_spacing (lsf_num, .true., plot_device, s)

        else if (command .eq. display_subband_cmd) then
            call display_subband (lsf_num, .true., plot_device, s)

        else if (command .eq. display_grey_cmd) then
            call display_grey (lsf_num, .true., s)

        else if (command .eq. display_fft_cmd) then
            call display_fft (lsf_num, plot_device, s)

        else if (command .eq. display_cont_fft_cmd) then
            call display_cont_fft (lsf_num, plot_device, s)

        else if (command .eq. grey_merge_cmd) then
            call grey_merge (lsf_num, .true., s)

        else if (command .eq. merge_spacings_cmd) then
            call merge_spacings (lsf_num, .true., plot_device, s)

        else if (command .eq. model_spacings_cmd) then
            call merge_spacings (lsf_num, .false., plot_device, s)

        else if (command .eq. print_spacings_cmd) then
            call print_spacings (lsf_num, s)

        else if (command .eq. scan_inter_cmd) then
            call scan_interference (lsf_num, plot_device, s)

        else if (command .eq. scan_sample_cmd) then
            if (tscope .eq. CLFST) then
               call scan_sample_clfst (lsf_num, s)
            else if (tscope .eq. RYLE) then
               call scan_sample_ryle (lsf_num, s)
            end if

        else if (command .eq. fit_spac_cmd) then
            call fit_spacings (lsf_num, plot_device, s)

        else if (command .eq. print_lsf_cmd) then
            call io_enqout (old_out)
            call io_opeout (out, s)
            call lsf_display (lsf_num, s)
            if (out .ne. old_out) then
                call io_close (out, s)
                call io_setout (old_out)
            end if

        else if (command .eq. list_sp_cmd) then
            call io_enqout (old_out)
            call io_opeout (out, s)
            call lsf_disspac (lsf_num, s)
            if (out .ne. old_out) then
                call io_close (out, s)
                call io_setout (old_out)
            end if

        else if (command .eq. print_obs_cmd) then
            call io_enqout (old_out)
            call io_opeout (out, s)
            call io_enqtio (termi,termo)
            if (s .eq. 0) then
               if (out.ne.termo) then
                   write (out,*)'PRINT-OBSERVATION'
                   write (out,*)' '
                   call io_lstfil (out, def_sf, s)
               endif
            endif


            call read_ct (sf_lun, s)
            if (tscope .eq. CLFST) then
               call exs_print_clfst ('PARAMETERS', s)
            else if (tscope .eq. RYLE) then
               call exs_print_ryle ('PARAMETERS', s)
            end if
            if (out .ne. old_out) then
                call io_close (out, s)
                call io_setout (old_out)
            end if

        else if (command .eq. flag_sys_cmd) then
            call flag_system (lsf_num, sf_lun, s)

        else if (command .eq. archive_flux_cmd) then
             call lsf_archive_flux (lsf_num, s)

        else if (command .eq. phase_fit_cmd) then
             call phase_fit (s)

        else
            ls = chr_lend (cmd_list (command), '.')
            call io_wrout (
     *        'Command '//cmd_list (command) (1:ls)//'not implemented.')

        end if

C         Restore status.
        if (s .eq. USR_BREAK) s = 0
        if (s .ne. 0) goto 9999

      if (command .ne. exit_cmd) goto 100

      call lsf_close (lsf_num, s)

C     Set the default lsf to the one that is now in common.
      def_lsf = 1

      call io_enqcli (string, ls)
      if (ls.eq.0) call io_wrout (' ')

      return

C Error Handling
C --------------
C
 9999 continue
          ls = 0
          call lsf_close (lsf_num, ls)
          call lsf_wrerr (s, 'in subroutine LSF_SYSTEM')
          return
      end
