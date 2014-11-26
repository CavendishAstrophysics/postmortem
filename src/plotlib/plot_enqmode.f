C
C
C+plot_enqmode
C
      subroutine plot_enqmode ( mode, s )
C
C Enquire the plot mode for use by plot_complex
C
C Returned:
C    Mode (normal or multiple)
       character*(*)      mode
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       mode = 'NORMAL'
       if (plot_mode.eq.plot_normal) then
         mode = 'NORMAL'
       elseif (plot_mode.eq.plot_multiple) then
         mode = 'MULTIPLE'
       elseif (plot_mode.eq.plot_brief) then
         mode = 'BRIEF'
       elseif (plot_mode.eq.plot_scaled_phi) then
         mode = 'SCALED-PHI'
       end if

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_ENQMODE' )

       end
