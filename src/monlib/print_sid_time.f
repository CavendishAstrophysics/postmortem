


*+PRINT_SID_TIME

       subroutine print_sid_time (file, status)
C      ----------------------------------------
C
C  Executes the PRINT-SIDEREAL-TIME command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the sidereal time associated with specified
C  samples from the current sample file.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
c
       character  line*50
       real*8     ra, dec
       integer*4  sid_time
       integer    ifile, iout, isamp, nsamp, isecs, itime(3)
       integer    length
c
       if (status.ne.0) return
c
c  Open the sample file
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
       call enq_numsamp ( ifile, 1, nsamp, status )
       if (status.eq.0) then
c
         call io_enqout(iout)
         write(iout,*)
         line='sample number : '
         line(24:36)='sidereal time'
         line(48:49)='ST'
c
c    Prompt for sample number
c
    1    isamp=0
         status=0
         call io_geti(line(1:16),' ',isamp,status)
         if (isamp.gt.nsamp) then
           write(iout,'(X,A,I5,A)')'*** only',NSAMP,' samples present'
c
         elseif (isamp.gt.0) then
           call read_rt(ifile,1,isamp,ra,dec,sid_time,status)
           if (status.eq.0) then
             isecs=sid_time/10
             call util_stohms(isecs,itime)
             call chr_chitoc(isamp,line(17:20),length)
             call chr_chljus(line(17:20),length)
             call chr_chtime(itime,line(39:46),length)
             write(iout,'(''+'',A)')line
           endif
         endif
c
         if (isamp.gt.0) goto 1
c
         if (status .eq. USR_BREAK) status = 0
         call close_source(ifile,1,status)
         call close_sf(ifile,status)
       endif
c
       if (status .ne. 0) then
         call mon_wrerr(status,'in routine PRINT_SID_TIME')
       end if
       write(iout,*)
c
       end
