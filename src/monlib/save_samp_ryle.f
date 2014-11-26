

*+SAVE_SAMP_RYLE

      subroutine save_samp_ryle ( file, status )
C
C  Executes the SAVE-SAMPLE-FILE command.
C
C  Given:
C      FILE          char*(*)      sample file name
C      STATUS        integer       status value
C
C  Splits a sample file containing data from a multi-centre observation
C  into individual sample files, each containing data for a single
C  pointing centre.  This command may be needed from time to time to
C  recover wreckage from aborted observations.
C
C  The STATUS value should be zero on entry.
C
C  (DJT, 8 June 92; GP 12 April 2000 [io_setacc])
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/ctab_pack.inc'
       include '/mrao/post/include/src_pack.inc'
       include '/mrao/post/include/samp_rt.inc'
c
       integer      buffer_size
       parameter   (buffer_size=4*1024)
       integer*4    buffer(buffer_size)
       common /post/ buffer
c
       character    source_file*64, dest_file*64
       integer      mod_samp, num_samp, samp_centre, samp_count
       integer      no_words, no_pages, max_pages, samp_pages
       integer      block_no, save_block, word_count, msamp
       integer      i, iout, isrce, idest, icentre
       integer      Rfile, Wfile
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
       call open_source(isrce,1,0,status)
       call Setbs(isrce,page)
c
       call enq_ctab_pack(isrce,ctab_pack,status)
       call enq_src_pack(isrce,1,src_pack,status)
       call enq_numsamp(isrce,1,num_samp,status)
       call enq_pages(isrce,max_pages,status)
c
       if (status.eq.0) then
c
         msamp = 0
         do icentre = 1, Ncentre
           msamp = msamp + tcentre(icentre)
         enddo
c
c    For each pointing centre
c
         do icentre = 1, Ncentre

c      Compute total sample count for this centre

           samp_count = (num_samp/msamp)*tcentre(icentre)
           mod_samp = mod(num_samp,msamp)
           do i = 1, icentre-1
             mod_samp = mod_samp - tcentre(i)
           enddo
           if (mod_samp.gt.0) samp_count = samp_count + mod_samp

c      Adjust control tables for this centre

           centre = icentre
           source = source_list(icentre)
           RAref = RAref_list(icentre)
           DECref = DECref_list(icentre)
           Nsamp = samp_count

           src_num_samp = samp_count
           do i = 1, ct_len_pack
             ctv2all(ct_src_ptr+i) = src_pack(i)
           enddo

c      Create and open the final destination sample file

           dest_file = file_list(icentre)
           samp_pages = src_samp_len/page
           no_pages = ct_pages + samp_count*samp_pages

           call io_crefil(dest_file,no_pages,.false.,2,status)
           call io_setacc(dest_file, 'r', 'rw', 'rw', status)
           call io_opefil(idest,dest_file,'D',0,status)
           call Setbs(idest,page)

c      Write control tables

           word_count = 0
           save_block = 0
           if (status.eq.0) then
             no_words = ct_pages*page
             status = Wfile(idest, 0, ctv2all, save_block, no_words)
             if (status.eq.0) then
               word_count = word_count + no_words
             endif
           endif

c      Write samples for this pointing centre

           block_no = ct_pages
           save_block = ct_pages
           no_words = samp_pages*page
           do while (block_no.lt.max_pages .and. status.eq.0)
             status = Rfile(isrce, 0, buffer, block_no, no_words)
             do i =  1, samp_rt_length
               samp_rt(i) = buffer(i)
             enddo
             samp_centre = ishft(samp_status,-12)
             if (samp_centre.eq.icentre .and. status.eq.0) then
               status = Wfile(idest, 0, buffer, save_block, no_words)
               save_block = save_block + samp_pages
               word_count = word_count + no_words
               RAdate = samp_ra
               DECdate = samp_dec
             endif
             block_no = block_no + samp_pages
           enddo

c      Write control tables, updated with precessed position

           if (status.eq.0) then
             save_block = 0
             no_words = ct_pages*page
             status = Wfile(idest, 0, ctv2all, save_block, no_words)
           endif

           if (status.eq.0) call Smax(idest, word_count*4-1)
           close (idest)

         enddo

         call close_source(isrce,1,status)
         call close_sf(isrce,status)

       endif

       if (status.ne.0 .and. status .ne. USR_BREAK) then
         call smp_wrerr( status, 'in subroutine SAVE_SAMP_RYLE' )
       endif

       write(iout,*)

       end
