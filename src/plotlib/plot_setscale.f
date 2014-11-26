C
C
C+plot_setscale
C
      subroutine plot_setscale ( auto_scaling, min_y, max_y, s )
C
C Sets the type and range for the plot y scale
C
C Input:
C    type of scaling
       logical            auto_scaling
C    range for y-scale
       real*4             min_y, max_y
C Returned:
C    Status
       integer            s
C-

       include '/mrao/post/include/plot_control.inc'
       include '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C decode character mode setting to integer variable
       if (auto_scaling) then
         plot_scale_type = 0
       else
         plot_scale_type = 1
         plot_limit_min  = min_y
         plot_limit_max  = max_y
       end if
       if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETSCALE' )

       end
