

*+PRINT_COLL_CLFST

       SUBROUTINE PRINT_COLL_CLFST (FILE, STATUS)
C      ------------------------------------------
C
C  Executes the PRINT-COLLIMATION-PHASES command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character  date*9
       integer    idate(3), length
       integer    ifile, iout, iold
       integer    termi, termo
c
       if (status.ne.0) return
c
       call io_enqout(iold)
       call io_enqtio(termi,termo)
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call io_opeout(iout,status)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-COLLIMATION-PHASES'
         write(iout,*)
         call io_lstfil(iout,file,status)
c
         idate(1)=icdat(1)
         idate(2)=icdat(2)
         idate(3)=icdat(3)
         call chr_chdate(idate,1,date,length)
c
         write(iout,'(X,A,A/)')'Last revision : ',date
         write(iout,'(X,A,4(/7F8.2),4(/8F8.2))')
     :                         'Collimation phase (degrees)',coll
       endif
c
       if (status.ne.0 .and. status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_COLL_CLFST')
       else
         status = 0
         call close_sf(ifile,status)
       endif
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
