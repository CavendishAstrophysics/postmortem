

*+PRINT_AMPF_CLFST

       SUBROUTINE PRINT_AMPF_CLFST (FILE, STATUS)
C      ------------------------------------------
C
C  Executes the PRINT-AMPLITUDE-FACTORS command.
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
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character  date*9
       integer    ifile, iout, iold, termi, termo
       integer    idate(3), length, lu
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
         if (iout.ne.termo) write(iout,*)'PRINT-AMPLITUDE-FACTORS'
         write(iout,*)
         call io_lstfil(iout,file,status)
c
         lu=chr_lenb(aunits)
         idate(1)=iadat(1)
         idate(2)=iadat(2)
         idate(3)=iadat(3)
         call chr_chdate(idate,1,date,length)
c
         write(iout,'(X,A,A/)')'Last revision : ',date
         write(iout,'(X,A,F8.2)')'Amplitude scaling factor :',ampscl
         write(iout,'(X,A,5X,F10.1,X,A/)')
     :                   'Amplitude cut-off :',achop*ampscl,aunits(1:lu)
         write(iout,'(X,A,4(/7F8.2),4(/8F8.2))')'Amplitude factors',ampf
       endif
c
       if (status.ne.0 .and. status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_AMPF_CLFST')
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
