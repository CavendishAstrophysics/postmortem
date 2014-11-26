C
C
C+ plot_getopts

      subroutine plot_getopts( plot_device, s )
C
C Set plot options interactively
C
C Returned:
C    default plot device
       character*(*)    plot_device
C    status word
       integer          s
C
C set plot options interactively.  The following options are defined:
C
C 1) AUTO-SCALING
C    turn auto-scaling of amplitude on
C
C 2) FIXED-SCALING
C    Parameters: lower_limit, upper_limit
C    define a fixed scaling for amplitude plots
C
C 3) PLOT-DEVICE
C    Parameters: device-name
C    define the plot device to use
C
C 4) PROMPT-STATE
C    Parameters: prompting-on/off
C    turn automatic prompting for output device on/off (default is off)
C
C 5) PHASE-LIMITS
C    Paramters: minimum-phase, maximum-phase
C    set the limits on the plots of phase
C
C 6) AMPLITUDE-PHASE-PLOTS
C    Parameters: none
C    plot amplitude and phase not cos/sin
C
C 7) COS-SIN-PLOTS
C    Parameters: none
C    plot cos and sin not amplitude/phase
C
C 8) BRIEF-DISPLAY
C    Parameters: on/off
C    turn brief display option on/off
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
C
C Local variables
C   options
       integer       number_options
       parameter    (number_options = 8)
       character*80  option_list(number_options), option

       data option_list(1)/
     * 'auto-scaling ......... turn auto-scaling of amplitude on'
     *                    /
       data option_list(2)/
     * 'fixed-scaling ........ define a fixed amplitude scale'
     *                    /
       data option_list(3)/
     * 'plot-device .......... specify a new default plot device'
     *                    /
       data option_list(4)/
     * 'prompt-state ......... use default-device (off) or prompt (on)'
     *                    /
       data option_list(5)/
     * 'phase-limits ......... limits on phase plots'
     *                    /
       data option_list(6)/
     * 'amplitude-phase-plot . plot amplitude and phase not cos/sin'
     *                    /
       data option_list(7)/
     * 'cos-sin-plots ........ plot cos and sin not amplitude/phase'
     *                    /
       data option_list(8)/
     * 'brief-display ........ turn brief display option on/off'
     *                    /

C check status on entry
       if (s.ne.0) return

C read option from the command line
       call io_getopt('Display-option (?=list) : ','auto-scaling',
     *             option_list,number_options,option,s)
       if (s.ne.0) goto 999


C decode options
       if (chr_cmatch(option,option_list(1))) then
         plot_scale_type = 0

       elseif (chr_cmatch(option,option_list(2))) then
         plot_scale_type = 1
         call io_getr('Amplitude lower-limit : ','0.0',plot_limit_min,s)
         call io_getr('Amplitude upper-limit : ','1.0',plot_limit_max,s)

       elseif (chr_cmatch(option,option_list(3))) then
          call io_getplt('Plot-device : ', '*', plot_device, s)

       elseif (chr_cmatch(option,option_list(4))) then
          if (io_onoff('Prompt-state : ','off',s)) then
            plot_prompt = 1
          else
            plot_prompt = 0
          end if

       elseif (chr_cmatch(option,option_list(5))) then
         call io_getr('Phase lower-limit : ','-180.0',plot_phi_min,s)
         call io_getr('Phase upper-limit : ','180.0',plot_phi_max,s)

       elseif (chr_cmatch(option,option_list(6))) then
         plot_data_type = plot_Ampphi

       elseif (chr_cmatch(option,option_list(7))) then
         plot_data_type = plot_cossin

       elseif (chr_cmatch(option,option_list(8))) then
         if (io_onoff('Brief output option : ','off',s)) then
           call plot_setmode('BRIEF',s)
         else
           call plot_setmode('NORMAL',s)
         end if

       end if

999   continue
      if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_GETOPTS' )
      end
