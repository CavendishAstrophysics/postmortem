

*+COPY_SAMP_RYLE

      subroutine copy_samp_ryle ( file, status )
C
C  Executes the COPY-SAMPLE-FILE command.
C
C  Given:
C      FILE          char*(*)      sample file name
C      STATUS        integer       status value
C
C  Copies the given sample file to a new destination (a contiguous
C  disc file of the appropriate length) and marks the sample file
C  as saved.  The STATUS value should be zero on entry.
C  last mod: GP 12 April 2000 (io_setacc)
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
c
       integer      buffer_size
       parameter   (buffer_size=16*1024)
       integer*4    buffer(buffer_size)
c
       common /post/ buffer
c
       character    tel_name*8
       character    source_file*64, dest_file*64
       integer      max_bytes, max_words, max_pages, max_block
       integer      iout, isrce, idest, block, words, word_count
       integer      tel_no
       integer      Smax, Rfile, Wfile
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
       call enq_tscope(isrce,tel_name,tel_no,status)
       call enq_pages(isrce,max_pages,status)
       call close_sf(isrce,status)
       max_bytes=max_pages*2048
c
c  Construct destination file name
c
       if (status.eq.0) then
         dest_file=file_list(1)
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
             call smp_wrerr(status,'in routine COPY_SAMP_RYLE')
           endif
         enddo
         status=Smax(idest,word_count*4-1)
         close(isrce)
         close(idest)
       endif

       if (status.ne.0 .and. status .ne. USR_BREAK) then
         call smp_wrerr( status, 'in subroutine COPY_SAMP_RYLE' )
       endif
c
       write(iout,*)
c
       end
