*$ Commands for the Interactive Data Editor
*  ----------------------------------------
C
C
          integer         num_cmds
          parameter     ( num_cmds = 10 )
          character*70    cmd_list( num_cmds )

          integer         plot_cmd
          parameter     ( plot_cmd = 1 )
          data            cmd_list(plot_cmd)
     * / 'plot-device ...... sets the current plot device' /

          integer         smooth_cmd
          parameter     ( smooth_cmd = 2 )
          data            cmd_list( smooth_cmd )
     * / 'smooth ........... boxcar smooths the data' /

          integer         interp_cmd
          parameter     ( interp_cmd = 3 )
          data            cmd_list(interp_cmd)
     * / 'interpolate ...... linearly interpolates across gaps' /

          integer         chop_cmd
          parameter     ( chop_cmd = 4 )
          data            cmd_list(chop_cmd)
     * / 'chop ............. rejects data outside a give range' /

          integer         smooth_chop_cmd
          parameter     ( smooth_chop_cmd = 5 )
          data            cmd_list(smooth_chop_cmd)
     * / 'varying-chop ..... chops data that deviates radically' /

          integer         range_cmd
          parameter     ( range_cmd = 6 )
          data            cmd_list(range_cmd)
     * / 'range ............ restricts the plot range' /

          integer         toggle_plot_cmd
          parameter     ( toggle_plot_cmd = 7 )
          data            cmd_list(toggle_plot_cmd)
     * / 'toggle_plot ...... toggles replotting on/off' /

          integer         point_cmd
          parameter     ( point_cmd = 8 )
          data            cmd_list(point_cmd)
     * / 'point ............ adds a point to the plot' /

          integer         write_cmd
          parameter     ( write_cmd = 9 )
          data            cmd_list(write_cmd)
     * / 'write ............ exits and saves the edited data' /

          integer         exit_cmd
          parameter     ( exit_cmd = 10 )
          data            cmd_list(exit_cmd)
     * / 'exit ............. exit' /

C         integer         #_cmd
C         parameter     ( #_cmd = 14 )
C         data            cmd_list(#_cmd)
C    * / '# ......' /

