C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_map(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_MAP (status)
C
C  Converts a map in MRAO format imported by binary ftp transfer from
C  the ND system.  Converts all floating-point redtape entries and
C  map data if necessary.
C
C  DJT, 15 Nov 96
*-
       integer    status
c
       character  file*64
       real*4     data(512)
       integer    ifile, irec
c
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/iolib_errors.inc'
c
       if (status.ne.0) return

c  Get map file name

       call io_getfil('map file : ', ' ', file, status)

c  Open the map file

       call io_nxtlun(ifile,status)
       open (ifile, file=file, access='DIRECT', status='OLD',
     :                         recl=1024*2, iostat=status)

c  Read map redtape from first record

       if (status.eq.0) then
         irec = 1
         read(ifile, rec=irec, iostat=status) irtall
       endif

       if (status.eq.0) then

c  Adjust block number for start of data

         mpblk1 = mpblk1+1

c  Convert floating point entries to IEEE format

         call util_rndr32(blank,blank,1)

         call util_rndr32(freq,freq,1)
         call util_rndr32(zerol,zerol,1)
         call util_rndr32(scalef,scalef,1)
         call util_rndr32(zmax,zmax,1)
         call util_rndr32(zmin,zmin,1)

         call util_rndr32(convpa,convpa,2)
         call util_rndr32(wghtpa,wghtpa,2)
         call util_rndr32(gradpa,gradpa,2)

         call util_rndr32(flxnrm,flxnrm,6)

         call util_rndr64(ramap,ramap,19)

c  Rewrite map redtape to first record

         irec = 1
         write(ifile, rec=irec, iostat=status) irtall
         write(*,'(I4,1X,A)')irec,'page of redtape converted'

       endif

c  Convert floating point data

       if (iswd.eq.3.or.iswd.eq.4) then

         do while (status.eq.0)

           irec = irec+1
           read(ifile, rec=irec, end=1, err=1, iostat=status) data
           call util_rndr32(data,data,512)
           write(ifile, rec=irec, iostat=status) data

         enddo

   1     irec = irec-2
         write(*,'(I4,1X,A)')irec,'pages of data converted'

       endif

       close(ifile)

       if (status.ne.0.and.status.ne.USR_BREAK) then
          call io_wrerr(status, 'in subroutine CONVERT_MAP')
       endif

       end
