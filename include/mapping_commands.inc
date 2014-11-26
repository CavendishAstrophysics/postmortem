C
C     Commands for the mapping section of POSTMORTEM
C
          integer         num_cmds
          parameter     ( num_cmds = 17 )

          character*70    cmd_list( num_cmds )

          integer         exit_cmd
          parameter     ( exit_cmd = 0 )

          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 1 )
          data            cmd_list( set_sample_file_cmd )
     * /'set-sample-file ..... initialises a map for a new sample file'/

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 2 )
          data            cmd_list( set_lsf_cmd )
     * / 'set-lsf ............. initialises a map for a new LSF' /

          integer         open_map_cmd
          parameter     ( open_map_cmd = 3 )
          data            cmd_list( open_map_cmd )
     * / 'open-map ............ opens an existing map' /

          integer         save_map_cmd
          parameter     ( save_map_cmd = 4 )
          data            cmd_list(save_map_cmd)
     * / 'save-map ............ saves the current map definition' /

          integer         print_redtape_cmd
          parameter     ( print_redtape_cmd = 5 )
          data            cmd_list(print_redtape_cmd)
     * / 'print-map-redtape ... prints the current redtape' /

          integer         make_map_cmd
          parameter     ( make_map_cmd = 6 )
          data            cmd_list(make_map_cmd)
     * / 'make-map ............ makes a map' /

          integer         select_conv_cmd
          parameter     ( select_conv_cmd = 7 )
          data            cmd_list(select_conv_cmd)
     * / 'convolution-func .... sets the convolution function to use' /

          integer         select_grading_cmd
          parameter     ( select_grading_cmd = 8 )
          data            cmd_list(select_grading_cmd)
     * / 'grading-function .... sets the grading function to use' /

          integer         select_weight_cmd
          parameter     ( select_weight_cmd = 9 )
          data            cmd_list(select_weight_cmd)
     * / 'weighting ........... sets the visibility weighting' /

          integer         select_size_cmd
          parameter     ( select_size_cmd = 10 )
          data            cmd_list(select_size_cmd)
     * / 'size ................ sets the map size' /

          integer         select_proj_cmd
          parameter     ( select_proj_cmd = 11 )
          data            cmd_list( select_proj_cmd )
     * / 'projection .......... sets the map projection parameters' /

          integer         select_gr_cent_cmd
          parameter     ( select_gr_cent_cmd = 12 )
          data            cmd_list(select_gr_cent_cmd)
     * / 'map-centre .......... sets the RA and dec of the grid (0,0)'/

          integer         select_uv_cent_cmd
          parameter     ( select_uv_cent_cmd = 13 )
          data            cmd_list(select_uv_cent_cmd)
     * / 'uv-centre ........... sets the map phase centre in u and v'/

          integer         select_type_cmd
          parameter     ( select_type_cmd = 14 )
          data            cmd_list(select_type_cmd)
     * / 'type ................ sets the map type' /

          integer         add_lsf_cmd
          parameter     ( add_lsf_cmd = 15 )
          data            cmd_list( add_lsf_cmd )
     * / 'add-lsf ............. includes another lsf in the map' /

          integer         delete_lsf_cmd
          parameter     ( delete_lsf_cmd = 16 )
          data            cmd_list(delete_lsf_cmd)
     * / 'delete-lsf .......... deletes an existing lsf from the map' /

          integer         plot_cmd
          parameter     ( plot_cmd = 17 )
          data            cmd_list(plot_cmd)
     * / 'plot-functions ...... plots the mapping functions' /

C         integer         #_cmd
C         parameter     ( #_cmd = 14 )
C         data            cmd_list(#_cmd)
C    * / '# ......' /


