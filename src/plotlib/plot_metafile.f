

*+plot_metafile

       subroutine plot_metafile( s )
C
C Plot a metafile on an output device
C
C Returned:
C   status
       integer       s
C
C-
       character*64  file,device
c
       if (s.ne.0) return

       file='PGPLOT:GMF'
       call io_enqplt(0,device)
       call io_getfil('metafile name : ','*',file,s)
       call io_getplt('graphics device/type : ','*',device,s)
       if (s.eq.0) then
          call gmfplot(file,device)
       endif
       if (s.ne.0) call io_wrerr( s, 'in plot_metafile' )
       end
