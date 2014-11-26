
*+PRINT_OBS_PARS

       subroutine print_obs_pars (file, status)
C      ----------------------------------------
C
C  Executes the PRINT-OBSERVATION command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the parameters from the current sample file
C  which describe the observation (map centre, integration time, etc).
C
C  DJT, 10/11/89
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
         if (iout.ne.termo) then
           write(iout,*)'PRINT-OBSERVATION'
           write(iout,*)' '
           call io_lstfil(iout,file,status)
         endif
       endif

       if (status.eq.0) then
         if (tscope.eq.'T151' .or. tscope.eq.'38MHZ') then
           call exs_print_clfst('PARAMETERS',status)
         elseif (tscope.eq.'RYLE') then
           call exs_print_ryle('PARAMETERS',status)
         else
           status=ILL_CONTTAB
         endif
       end if

       if (status.ne.0.and.status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_OBS_PARS')
       else
         status = 0
         call close_sf(ifile,status)
       endif

       call io_close(iout,status)
       call io_setout(iold)

       end
