

*+PRINT_CONFIG_RYLE

       subroutine print_config_ryle (file, status)
C      -------------------------------------------
C
C  Executes the PRINT-CONFIGURATION command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the telescope configuration for the specified
C  sample file.
C
C
C  PA, 6/3/89
C  DJT, 14/4/92
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/control_tables.inc'

       real*8        ch_freq(max_channel)
       integer       ifile, iout, iold, termi, termo
       integer       isp, iae, iba, ich, nsp, nae, nba, nch
       integer       list(1000)
       character*1   band(5)
       data          band / 'A', 'B', 'C', 'D', 'E'/

       if (status.ne.0) return

C  Open the sample file and read control tables

       call open_sf(ifile,file,'read',0,status)

C  Prompt for output file

       call io_enqout(iold)
       call io_opeout(iout,status)
       call io_enqtio(termi,termo)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-CONFIGURATION'
         write(iout,*)' '
         call io_lstfil(iout,file,status)
       endif

       if (status.eq.0) then

         call enq_obsdef( ifile, nae, nsp, nba, nch, status)
         if (status.eq.0) then
           write(iout,*)'Aerials:'
         end if
         do iae = 1,nae
           call enq_iae_code ( ifile, iae, list(iae), status )
         end do
         call print_list ( list, nae, status )
         if (status.ne.0) goto 999
         write (iout,*)'Spacings:'
         do isp = 1,nsp
           call enq_isp_code( ifile, isp, list(isp), status )
         end do
         call print_list( list, nsp, status )
         if (status.ne.0) goto 999
         write (iout,*)'Sub-Bands:'
         do iba = 1,nba
           call enq_iba_code( ifile, iba, list(iba), status )
         end do
         if (status.ne.0) goto 999
         write (iout,'(1X,5(4X,A1))') (band(list(iba)), iba=1,nba)
         write (iout,*)
         write (iout,*)'Channels:'
         do ich = 1,nch
           call enq_ich_code( ifile, ich, list(ich), status )
         end do
         call print_list( list, nch, status )
         write (iout,*)'Frequencies:'
         do iba = 1,nba
           do ich = 1,nch
             call enq_chfreq( ifile, iba, ich, ch_freq(ich), status )
             ch_freq(ich)=ch_freq(ich)/1.E6
           enddo
           write (iout,'(8F10.2)')(ch_freq(ich),ich=1,nch)
         enddo
       end if


999    continue
       if (status.ne.0.and.status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_CONFIG')
       end if

       status = 0
       call close_sf(ifile,status)
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)' '

       end
