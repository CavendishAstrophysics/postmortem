C
C
C+plot_setmode
C
      subroutine plot_setmode ( mode, s )
C
C Sets the plot mode for use by plot_complex
C
C Input:
C    Mode (normal or multiple)
       character*(*)      mode
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C local function
       logical   chr_cmatch

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       if (chr_cmatch(mode,'NORMAL')) then
         plot_mode = plot_normal
       else if (chr_cmatch(mode,'MULTIPLE')) then
         plot_mode = plot_multiple
       else if (chr_cmatch(mode,'BRIEF')) then
         plot_mode = plot_brief
       else if (chr_cmatch(mode,'SCALED-PHI')) then
         plot_mode = plot_scaled_phi
       else
         plot_mode = ill_pltmode
       end if
C reset page number to the start of the plot
       plot_page = 1

       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETMODE' )

       end
