C
C
C+plot_begin

      subroutine plot_begin( plot_device, s )
C     ---------------------------------------
C
C open the PGPLOT plot device
C
C Given:
C   default plot device
       character*(*)    plot_device
C returned
C   error return
       integer          s
C
C-
C local plot device
       character*40  local_device

       include '/mrao/post/include/plot_control.inc'

C check status on entry
       if (s.ne.0) return

C examine mode and take action
       if (plot_prompt.eq.1) then
          call io_getplt('Plot-device : ', plot_device, local_device, s)
          call pgbegin( 0, local_device, 1, 1 )
       else
          call pgbegin( 0, plot_device, 1, 1 )
       end if

C report any errors
       if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_BEGIN' )

       end
