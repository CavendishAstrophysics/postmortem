C
C
C+plot_setvp

       subroutine plot_setvp( nx, ny, vp, s )
C      --------------------------------------
C
C Find suitable values for viewports in a multiple plot
C
C Input:
C    Number of segments in x and y
       integer       nx, ny
C Returned:
C    View-ports
       real*4        vp(4,2,*)
C    Status
       integer       s
C-

C Local variables
       integer     ntot, n1, n2
       real*4      size_x, size_y

C check status on entry
       if ( s.ne.0 ) return

C move through the range of windows
       ntot = 0
       size_x = .9 / float(nx)
       size_y = .75 / float(ny)
       do n1 = 1, nx
         do n2 = 1, ny
           ntot = ntot + 1
           if (n1.eq.1) then
             vp(1,1,ntot) = 0.05 + (n1-1)*size_x + size_x*0.075
             vp(1,2,ntot) = vp(1,1,ntot)
           else
             vp(1,1,ntot) = 0.05 + (n1-1)*size_x
             vp(1,2,ntot) = vp(1,1,ntot)
           end if
           vp(2,1,ntot) = 0.05 + n1*size_x - size_x*0.075
           vp(2,2,ntot) = vp(2,1,ntot)
           vp(4,1,ntot) = 0.80 - (n2-1)*size_y - 0.075*size_y
           vp(3,1,ntot) = vp(4,1,ntot) - size_y/2.0 + 0.125*size_y
           vp(3,2,ntot) = 0.80 - n2*size_y + 0.075*size_y
           vp(4,2,ntot) = vp(3,2,ntot) + size_y/2.0 - 0.125*size_y
         end do
       end do

       if (s.ne.0) call plot_wrerr( s, 'in subroutine PLOT_SETVP')
       end
