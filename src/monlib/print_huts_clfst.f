

*+PRINT_HUTS_CLFST

       subroutine print_huts_clfst (file, status)
C      ------------------------------------------
C
C  Executes the PRINT-HUT-STATUS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the hut status information recorded during
C  the observing run.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       integer    ifile, iout, iold, ihut, nhut_bad
       integer    termi, termo
c
       if (status.ne.0) return
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call close_sf(ifile,status)
c
c  Prompt for further options and output file
c
       call io_enqout(iold)
       call io_opeout(iout,status)
       call io_enqtio(termi,termo)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-HUT-STATUS'
         write(iout,*)
         call io_lstfil(iout,file,status)
       endif
c
       if (status.eq.0) then
c
         if (istop.eq.0) write(iout,'(X,A/)')
     :       'Hut status at the beginning of the observing run:'
c
         call check_huts_clfst(nhut_bad,status)
c
         write(iout,'(8O8)')(ihutstat(ihut),ihut=1,max_huts)
c
       endif
c
       if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine PRINT_HUTS_CLFST')
       endif
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
