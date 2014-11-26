C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call convert_ion(status)
       call io_setesc(.false.)
       end



*+
       subroutine CONVERT_ION (status)
C
C  Converts an ionospheric correction file imported to Unix from ND systems
C
C  DJT, 15 July 92
*-
       integer    status
c
       include '/mrao/post/include/ion_definition.inc'
c
       character  string*64, file*64
       integer    ifile, irec, lf
c
       if (status.ne.0) return

c  Get ionospheric correction file name

       call io_getfil('ion correction file : ', ' ', string, status)
       call io_makfil(' ',string,'ION',file,lf)

c  Open the ionospheric correction file

       call io_nxtlun(ifile,status)
       open (ifile, file=file, access='DIRECT', status='OLD',
     :                         recl=ion_length*4, iostat=status)

c  Read the correction definition, convert floating-point variables to
c  IEEE format and rewrite.

       if (status.eq.0) then
         irec = 1
         read(ifile, rec=irec, iostat=status) ion_redtape 
         if (status.eq.0) then
           call util_rndr32(ion_factor,ion_factor,1)
           call util_rndr64(ion_ra,ion_ra,max_ionsrcs)
           call util_rndr64(ion_dec,ion_dec,max_ionsrcs)
           write(ifile, rec=irec, iostat=status) ion_redtape
         endif
       endif

       close(ifile)

       if (status.ne.0) then
          call io_wrerr(status, 'in subroutine CONVERT_ION')
       endif

       end
