*$(2)  Auxiliary routines


*+LOG_COMMENT

       subroutine log_comment (file, status)
C      -------------------------------------
C
C  Adds text comment to the observation log.
C
C  Given:
C      FILE          char*(*)      log file name
C      STATUS        integer       status value
C
C  Prompts for lines of text which are appended to the observation
C  log file.  The STATUS value should be zero on entry.
C
*-
       character  file*(*)
       character  line*80
       integer    ifile, l, chr_lenb, nl, status
c
       if (status.ne.0) return
c
       call io_opefil(ifile,file,'WA',0,status)
       if (status.eq.0) then
         nl=0
    1    line=' '
         call io_getstr('comment:',' ',line,status)
         l=chr_lenb(line)
         if (l.gt.0 .and. status.eq.0) then
           nl=nl+1
           if (nl.eq.1) write(ifile,*)
           write(ifile,*)line(1:l)
           goto 1
         endif
         close(ifile)
       endif
c
       end
