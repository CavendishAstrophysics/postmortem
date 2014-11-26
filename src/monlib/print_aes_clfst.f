


*+PRINT_AES_CLFST

       subroutine print_aes_clfst (file, status)
C      -----------------------------------------
C
C  Executes the PRINT-AERIAL-STATUS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the aerial status information recorded during
C  the observing run.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       character  line*80
       integer    iae, iae1, iae2, ihut, nae_bad
       integer    ifile, iout, iold, l, chr_lenb
       integer    termi, termo
       logical    full
c
       if (status.ne.0) return
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
c
c  Prompt for further options and output file
c
       full=io_yesno('Do you want the full status words? ','no',status)
       call io_enqout(iold)
       call io_opeout(iout,status)
       call io_enqtio(termi,termo)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'PRINT-AERIAL-STATUS'
         write(iout,*)
         call io_lstfil(iout,file,status)
       endif
c
       if (status.eq.0) then
c
         if (istop.eq.0) write(iout,'(X,A/)')
     :       'Aerial status at the beginning of the observing run:'
c
c    Print receiving status
c
         do ihut=1,max_huts
           call enq_ae_hut(ifile,ihut,iae1,iae2,status)
           write(line,'(20X,8I5)')(iae,iae=iae1,iae2)
           if (ihut.eq.1) line(1:20)='Aerials receiving:'
           do iae=iae1,iae2
             l=(iae-iae1)*5+24
             if (btest(iaestat(iae),14).eq.0) line(l:l+1)=' .'
           enddo
           write(iout,*)line(1:chr_lenb(line))
         enddo
         write(iout,*)
c
c    Print tracking status
c
         do ihut=1,max_huts
           call enq_ae_hut(ifile,ihut,iae1,iae2,status)
           write(line,'(20X,8I5)')(iae,iae=iae1,iae2)
           if (ihut.eq.1) line(1:20)='Aerials tracking:'
           do iae=iae1,iae2
             l=(iae-iae1)*5+24
             if (btest(iaestat(iae),13).eq.0) line(l:l+1)=' .'
           enddo
           write(iout,*)line(1:chr_lenb(line))
         enddo
         write(iout,*)
c
c    Report any pointing errors
c
         call check_aes_clfst(ifile,nae_bad,status)
c
c    Print the full status words
c
         if (full) then
           write(iout,*)'Full status words:'
           do ihut=1,max_huts
             call enq_ae_hut(ifile,ihut,iae1,iae2,status)
             write(iout,'(X,A,I3,A,I2,2X,8O8)')
     :         'ae',iae1,'-',iae2,(iaestat(iae),iae=iae1,iae2)
           enddo
         endif
       endif
c
       if (status.ne.0 .and. status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_AES_CLFST')
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
