C
C
C+plot_setpage

      subroutine plot_setpage( items, s )
C
C Set the number of items needed on a single plot in multiple plot mode
C
C Input:
C    Number of plots needed
       integer           items
C Returned:
C    Status
       integer           s
C-

       include   '/mrao/post/include/plot_control.inc'
       include   '/mrao/post/include/plot_errors.inc'

C check status on entry
       if (s.ne.0) return

C check for a call to plot_setmode
       if (plot_mode.ne.plot_multiple) then
         call io_wrout( '*** No call to PLOT_SETMODE ( multiple ) ' )
         s = ill_pltmode
         goto 999
       end if

C check number of items does not exceed maximum
       if (items.gt.9) then
         s = ill_pltmlt
       else
         full_page = items
         plot_page = 1
         if (items.eq.1) then
           segment_x = 1
           segment_y = 1
         else if (items.eq.2) then
           segment_x = 1
           segment_y = 2
         else if (items.le.4) then
           segment_x = 2
           segment_y = 2
         else if (items.le.6) then
           segment_x = 2
           segment_y = 3
         else
           segment_x = 3
           segment_y = 3
         end if
       end if

999    if (s.ne.0) call plot_wrerr(s,'in subroutine PLOT_SETPAGE')

       end
