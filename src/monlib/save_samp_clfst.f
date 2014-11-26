
*+SAVE_SAMP_CLFST

      subroutine save_samp_clfst ( file, status )
C
C  Executes the SAVE-SAMPLE-FILE command.
C
C  Given:
C      FILE          char*(*)      sample file name
C      STATUS        integer       status value
C
C  Copies the given sample file to a new destination (a contiguous
C  disc file of the appropriate length) and marks the sample file
C  as saved.  The STATUS value should be zero on entry.
C last mod (GP) 12 April 2000 [io_setacc]
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
       integer      buffer_size
       parameter   (buffer_size=16*1024)
       integer*4    buffer(buffer_size)
       common /post/ buffer
c
       character    string*80, user*16, tel_name*8
       character    source_file*64, dest_file*64, log_file*32
       integer      max_bytes, max_words, max_pages, max_block
       integer      ifile, iout, isrce, idest, block, words, word_count
       integer      lt, length, tel_no
       integer      Rmax, Smax, Rfile, Wfile
c
       if (status.ne.0) return
c
       call io_enqout(iout)
c
c  Confirm source sample file name
c
       call io_namfil(file,source_file,0,status)
       call io_getc('sample file:','*',source_file,status)
c
c  Load control tables for this file, find length of file in pages
c
       call open_sf(isrce,source_file,'read',0,status)
       if (status.eq.0) then
         status=Rmax(isrce,max_bytes)
         max_pages=max_bytes/2048
         call enq_tscope(isrce,tel_name,tel_no,status)
         call close_sf(isrce,status)
c
         if (ct_vers.eq.0 .and. isaved.gt.0) then
           if (.not.io_yesno('... sample file already saved, '//
     :       'do you want to save it again?  ','no',status)) return
         endif
       endif
c
c  Construct destination file name
c
       if (status.eq.0) then
         lt=chr_lenw(title)
         string=title(1:lt)//'-'
         call chr_chucas(string(1:lt))
         write(string(lt+2:),'(3I2.2)')
     :                           idat1(1),idat1(2),mod(idat1(3),1900)
c
         call enq_sf_dir(tel_name,user,status)
c        if (tel_name.eq.'T151') user='SAMP-ONE:T151'
c        if (tel_name.eq.'38MHZ') user='SAMP-ONE:38MHZ'
         call io_makfil(user,string(1:lt+9),'SAMP',dest_file,length)
         call io_getfil('save file:','*',dest_file,status)
       endif
c
c  Create destination file, of the appropriate size
c
       call io_crefil(dest_file,max_pages,.false.,2,status)
       call io_setacc(dest_file, 'r', 'rw', 'rw', status)
c

c  Copy from source to destination, via local buffer
c
       call io_opefil(isrce,source_file,'D',0,status)
       call io_opefil(idest,dest_file,'D',0,status)
       if (status.eq.0) then
c
         max_words=max_bytes/4
         max_block=max_words/buffer_size
         call Setbs(isrce,buffer_size)
         call Setbs(idest,buffer_size)
         write(*,*)'copying to file '//dest_file
c
         block=0
         word_count=0
         do while (status.eq.0 .and. block.le.max_block)
           words=buffer_size
           if (block.eq.max_block) words=mod(max_words,buffer_size)
           status=Rfile(isrce,0,buffer,block,words)
           if (status.eq.0) status=Wfile(idest,0,buffer,block,words)
           if (status.eq.0) then
             word_count=word_count+words
             block=block+1
           else
             call smp_wrerr(status,'in routine SAVE_SAMP_CLFST')
           endif
         enddo
         status=Smax(idest,word_count*4-1)
         close(isrce)
         close(idest)
       endif
c
       if (ct_vers.eq.0 .and. isaved.eq.0) then
c
c    Write entry into observation log
c
         if (status.eq.0) then
           log_file='(TELESCOPE)OBSERVATION-LOG:SYMB'
           call io_opefil(ifile,log_file,'WA',0,status)
           if (status.eq.0) then
             write(ifile,*)'sample file saved on '//dest_file(1:length)
             close(ifile)
           endif
           call log_comment(log_file,status)
         endif
c
c    Mark source file as saved
c
         call open_sf(ifile,source_file,'write',0,status)
         if (status.eq.0) then
           isaved=1
           call write_ct(ifile,status)
           call close_sf(ifile,status)
         else if (status .ne. USR_BREAK) then
           call smp_wrerr( status, 'in subroutine SAVE_SAMP_CLFST' )
         endif
c
       endif
c
       write(iout,*)
c
       end
