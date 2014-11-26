

*+PRINT_CT_CLFST

       subroutine print_ct_clfst (file, status)
C      ----------------------------------------
C
C  Executes the PRINT-CONTROL-TABLES command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the details of the real-time correlator control
C  tables, describing the correlator duty cycle, phase switching, etc.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character  line*80
       integer    ifile, iout, iold, it1, it2
       integer    termi, termo
c
       if (status.ne.0) return
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call close_sf(ifile,status)
c
c  Prompt for output file
c
       call io_enqout(iold)
       call io_opeout(iout,status)
       call io_enqtio(termi,termo)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-CONTROL-TABLES'
         write(iout,*)
         call io_lstfil(iout,file,status)
       endif
c
       if (status.eq.0) then
c
         it1=itab(1)/10
         it2=(itab(1)+itab(2))/10
         write(iout,1)it2
         write(iout,2)it1
         write(iout,3)it2-it1
         write(iout,4)izero
         write(line,5)
         if (btest(iphsw,1).eq.0) line(37:40)=' OFF'
         write(iout,*)line(1:40)
         write(line,6)
         if (btest(iphsw,2).eq.0) line(37:40)=' OFF'
         write(iout,*)line(1:40)
         write(line,7)
         if (istow.eq.0) line(37:40)=' OFF'
         write(iout,*)line(1:40)
         write(line,8)
         if (iutim.eq.1) line(38:40)='BST'
         write(iout,*)line(1:40)
         write(line,9)
         if (iosc.eq.2) line(1:2)='Ex'
         write(iout,*)line(1:28)
c
    1    format(' Sampling periodicity ',11('.'),I5,' ms')
    2    format('  integration time ',14('.'),I5,' ms')
    3    format('  read time ',21('.'),I5,' ms')
    4    format(' Correlator zero-level ',12('.'),I6)
    5    format('Half-wave switching ',17('.'),' ON')
    6    format('Quarter-wave switching ',14('.'),' ON')
    7    format('Auto-stow switched ',18('.'),' ON')
    8    format('Local time is ',22('.'),' GMT')
    9    format('Internal oscillator selected')
c
       else if (status .ne. USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_CT_CLFST')
       endif
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end
