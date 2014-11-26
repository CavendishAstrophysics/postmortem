

*+SET_COMMENT_RYLE

       subroutine set_comment_ryle (file, status)
C      ------------------------------------------
C
C  Executes the SET-COMMENT command for RYLE sample files.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to update comment in the sample file header.
C  Up to 5 lines of text comment may be held with the sample file.
C
C  DJT, 12/2/90
*-
       character  file*(*)
       integer    status

       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'

       integer       ifile, i, lc

       if (status.ne.0) return

C  Open the sample file and read control tables

       call open_sf(ifile,file,'write',0,status)
       call read_ct(ifile,status)

       if (status.eq.0) then

C    Display existing comment

         i = 0
         lc = 1
         do while (i.lt.5 .and. lc.gt.0)
           i = i + 1
           lc = chr_lenb(comment_lines(i))
           if (lc.gt.0) then
             if (i.eq.1) call io_wrout(' ')
             call io_wrout(comment_lines(i)(1:lc))
           endif
         enddo
         if (lc.gt.0) i = i + 1
         call io_wrout(' ')

C    Prompt for further comment lines

         if (i.gt.1) then
           if (io_yesno('Do you want to delete existing comment?', 'no',
     :                                                     status)) then
             do i = 1,5
               comment_lines(i) = ' '
             enddo
             i = 1
           endif
           call io_wrout(' ')
         endif

         if (i.le.5) then
           do while (i.le.5 .and. status.eq.0)
             call io_getstr('comment:', ' ', comment_lines(i), status)
             if (chr_lenb(comment_lines(i)).eq.0) status = USR_BREAK
             i = i + 1
           enddo
           call io_wrout(' ')
         endif
         status = 0

C    Write control tables and close sample file

         call write_ct(ifile,status)
         call close_sf(ifile,status)

       end if

       if (status.ne.0) then
         call mon_wrerr(status,'in routine SET_COMMENT_RYLE')
       endif

       end
