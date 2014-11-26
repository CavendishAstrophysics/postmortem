C
C     Commands for the remove system section of POSTMORTEM
C
          integer         num_cmds
          parameter     ( num_cmds = 8 )

          character*70    cmd_list( num_cmds )

          integer         exit_cmd
          parameter     ( exit_cmd = 0 )

          integer         set_sample_file_cmd
          parameter     ( set_sample_file_cmd = 1 )
          data            cmd_list( set_sample_file_cmd )
     *  / 'set-sample-file ..... initialises a remove for another file'/

          integer         set_lsf_cmd
          parameter     ( set_lsf_cmd = 2 )
          data            cmd_list( set_lsf_cmd )
     *  / 'set-lsf ............. initialises a remove for another LSF' /

          integer         open_rem_cmd
          parameter     ( open_rem_cmd = 3 )
          data            cmd_list(open_rem_cmd)
     * / 'open-remove ......... selects an existing remove to open' /

          integer         save_rem_cmd
          parameter     ( save_rem_cmd = 4 )
          data            cmd_list(save_rem_cmd)
     * / 'save-remove ......... saves the remove to be made later' /

          integer         display_rem_cmd
          parameter     ( display_rem_cmd = 5 )
          data            cmd_list( display_rem_cmd )
     * / 'print-remove ........ prints the lsf for the remove' /

          integer         make_rem_cmd
          parameter     ( make_rem_cmd = 6 )
          data            cmd_list(make_rem_cmd)
     * / 'make-remove ......... makes all outstanding removes' /

          integer         interactive_rem_cmd
          parameter     ( interactive_rem_cmd = 7 )
          data            cmd_list(interactive_rem_cmd)
     * /'go-remove ........... makes outstanding removes on line' /

          integer         model_rem_cmd
          parameter     ( model_rem_cmd = 8)
          data            cmd_list(model_rem_cmd)
     * / 'model-source ........ defines a source model' /

C         integer         #_cmd
C         parameter     ( #_cmd = 14 )
C         data            cmd_list(#_cmd)
C    * / '# ......' /


