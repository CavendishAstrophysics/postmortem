C
C     Commands for the ionospheric correction section of POSTMORTEM
C
          integer         num_cmds
          parameter     ( num_cmds = 16 )

          character*70    cmd_list( num_cmds )

          integer         exit_cmd
          parameter     ( exit_cmd = 0 )

          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 1 )
          data            cmd_list( set_sample_file_cmd )
     * /'set-sample-file ..... initialises a correction for a new file'/

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 2 )
          data            cmd_list( set_lsf_cmd )
     * / 'set-lsf ............. initialises a correction for a new LSF'/

          integer         open_ion_cmd
          parameter     ( open_ion_cmd = 3 )
          data            cmd_list(open_ion_cmd)
     * / 'open-correction ..... selects an existing correction' /

          integer         save_ion_cmd
          parameter     ( save_ion_cmd = 4 )
          data            cmd_list(save_ion_cmd)
     * / 'save-correction ..... saves this correction to be made later'/

          integer         print_correction_cmd
          parameter     ( print_correction_cmd = 5 )
          data            cmd_list(print_correction_cmd)
     */'print-correction .... prints the current correction definition'/

          integer         make_ion_cmd
          parameter     ( make_ion_cmd = 6 )
          data            cmd_list(make_ion_cmd)
     * / 'make-correction ..... makes all outstanding corrections' /

          integer         interactive_ion_cmd
          parameter     ( interactive_ion_cmd = 7 )
          data            cmd_list(interactive_ion_cmd)
     * / 'go-correction ....... makes corrections on line'/

          integer         print_all_cmd
          parameter     ( print_all_cmd = 8 )
          data            cmd_list( print_all_cmd )
     * / 'list-corrections .... lists all the corrections on file'/

          integer         add_source_cmd
          parameter     ( add_source_cmd = 9 )
          data            cmd_list(add_source_cmd)
     * / 'add-source .......... adds a source to the correction' /

          integer         delete_source_cmd
          parameter     ( delete_source_cmd = 10 )
          data            cmd_list(delete_source_cmd)
     * / 'delete-source ....... deletes a source from the correction' /

          integer         change_parameters_cmd
          parameter     ( change_parameters_cmd = 11 )
          data            cmd_list(change_parameters_cmd)
     * / 'change-parameters ... changes other correction parameters' /

          integer         display_ion_corr_cmd
          parameter     ( display_ion_corr_cmd = 12 )
          data            cmd_list(display_ion_corr_cmd)
     * / 'display-correction .. displays all the correction' /

          integer         display_ion_samp_cmd
          parameter     ( display_ion_samp_cmd = 13 )
          data            cmd_list(display_ion_samp_cmd)
     * / 'display-sample ...... displays a sample of the correction' /

          integer         edit_ion_corr_cmd
          parameter     ( edit_ion_corr_cmd = 14 )
          data            cmd_list(edit_ion_corr_cmd)
     * / 'edit-correction ..... interactive editing of a correction' /

          integer         display_ion_spectrum_cmd
          parameter     ( display_ion_spectrum_cmd = 15 )
          data            cmd_list(display_ion_spectrum_cmd)
     * / 'display-ion-spectrum  displays spectrum of correction phases'/

          integer         display_wavefront_cmd
          parameter     ( display_wavefront_cmd = 16 )
          data            cmd_list(display_wavefront_cmd)
     * / 'display-wavefront ... display aerial-merged phases vs x-geom'/

C         integer         #_cmd
C         parameter     ( #_cmd = 14 )
C         data            cmd_list(#_cmd)
C    * / '# ......' /

