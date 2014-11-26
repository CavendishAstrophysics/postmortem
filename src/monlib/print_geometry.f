

*+PRINT_GEOMETRY

       subroutine print_geometry (file, status)
C      ----------------------------------------
C
C  Executes the PRINT-GEOMETRY command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the current geometry.
C
C  PA, 26/2/89
C  DJT, 10/10/89
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/control_tables.inc'

       character     tscope*6
       integer       ifile, iout, iold, i
       integer       termi, termo

       if (status.ne.0) return

C  Open the sample file and read control tables

       call open_sf(ifile,file,'read',0,status)
       call enq_tscope(ifile,tscope,i,status)

C  Prompt for output file

       call io_enqout(iold)
       call io_opeout(iout,status)
       call io_enqtio(termi,termo)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-GEOMETRY'
         write(iout,*)' '
         call io_lstfil(iout,file,status)
       endif

       if (status.eq.0) then
         if (tscope.eq.'T151' .or. tscope.eq.'38MHZ') then
           call print_geom_clfst(ifile,status)
         elseif (tscope.eq.'RYLE') then
           call print_geom_ryle(ifile,status)
         else
           status=ILL_CONTTAB
         endif
       end if

       if (status.ne.0.and.status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_GEOMETRY')
       else
         status = 0
         call close_sf(ifile,status)
       endif

       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)' '

       end
