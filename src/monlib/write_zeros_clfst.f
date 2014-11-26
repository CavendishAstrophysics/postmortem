

*+WRITE_ZEROS_CLFST

       subroutine write_zeros_clfst (file, status)
C      -------------------------------------------
C
C  Updates the zero-correction file.
C
C  Given:
C      FILE          char*(*)      calibration source sample file name
C      STATUS        integer       status value
C
C  Routine to update the data on the zero-correction file, using new
C  correction data recorded during a calibration source observation.
C  The corrections are derived during the observation as a simple
C  average visibility for each active spacing, and are dumped as a
C  record at the end of the sample file.  This routine uses these new
C  corrections to adjust the values on the 'zero-correction file' for
C  use during subsequent observations.
C last mod 12 April 2000 GP (io_setacc)
*-
       character  file*(*)
       integer    status
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  tscope_name*5, zfile*40
       integer    i, is, nsp
       integer    ifile
c
c  Workspace
c
       real      rcos(max_spac), rsin(max_spac)
       real      data(2,max_spac)
c
       common /post/ rcos, rsin, data
c
c
       if (status.ne.0) return
c
c  Open the sample file and check telescope type
c
       call open_sf( ifile, file, 'read', 0, status)
       if (status.ne.0) goto 9999
c
       call enq_tscope( ifile, tscope_name, i, status )
       if (tscope_name.ne.'T151' .and. tscope_name.ne.'38MHZ') then
         status = ILL_TSCOPE
         goto 9999
       end if
c
c  Read the zero correction block
c
       call read_zero_corr( ifile, data, status )
       if (status.ne.0) goto 9999
c
       call enq_numsp( ifile, nsp, status )
       call enq_namfil( ifile, 'ZERO', zfile, status )
c
       do is=1,nsp
         rcos(is)=data(1,is)
         rsin(is)=data(2,is)
       enddo
c
c  Close the sample file
c
       call close_sf( ifile, status )
c
c
c  Read the existing corrections from disc and adjust
c
       call io_opefil( ifile, zfile, 'read', 0, status )
       if (status.eq.0) then
         read(ifile)((data(i,is),i=1,2),is=1,nsp)
         close(ifile)
         do is=1,nsp
           data(1,is)=data(1,is)+rcos(is)
           data(2,is)=data(2,is)+rsin(is)
         enddo
c
c    Write corrections back to disc
c
         call io_getfil( 'New zero-correction file:', '*', zfile,
     :                                                      status )
         call io_opefil( ifile, zfile, 'write', 0, status)
         call io_setacc(zfile, 'r', 'rw', 'rw', status)
         if (status.eq.0) then
           write(ifile)((data(i,is),i=1,2),is=1,nsp)
           close(ifile)
         endif
       endif
c
 9999  if (status.ne.0 .and. status .ne. USR_BREAK) then
         call mon_wrerr( status, 'in routine WRITE_ZEROS_CLFST' )
       endif
c
       end
