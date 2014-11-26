

*+PRINT_ZEROS_CLFST

       subroutine print_zeros_clfst (file, status)
C      -------------------------------------------
C
C  Executes the PRINT-ZERO-CORRECTION command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the zero corrections recorded with the current
C  sample file.  These corrections are derived during the observation
C  and are dumped as a record at the end of the sample file.  They may
C  be used to adjust the values on the 'zero-correction file' for use
C  during subsequent observations.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  list*80, option*8, tscope_name*5
       integer    i, iae, iae1, iae2, is, n, nae, ns
       integer    ifile, iout, iold, termi, termo
       real       zmin, zmax, zmean, zstdev
       real*8     sum, sumsq
c
c  Workspace
c
       integer   nn(max_aes), ilist(max_spac)
       real      rcos(max_spac), rsin(max_spac)
       real      data(2,max_spac)
c
       common /post/ data, rcos, rsin, ilist, nn
c
       if (status.ne.0) return
c
       call io_enqtio(termi,termo)
       call io_enqout(iold)
       iout=iold
c
c  Open the sample file and read the zero correction block
c
       call open_sf(ifile,file,'read',0,status)
       if (status.ne.0) goto 9999
c
c  Check telescope type
c
       call enq_tscope( ifile, tscope_name, i, status )
       if (tscope_name.ne.'T151' .and. tscope_name.ne.'38MHZ') then
         status = ILL_TSCOPE
         return
       end if
c
       call read_zero_corr( ifile, data, status)
       if (status.ne.0) goto 9999
c
c
c  Prompt for print options and output file
c
       if (io_yesno
     :     ('Do you want to print out the zero-corrections?','no',
     :                                                     status)) then
         option=' '
         if (io_yesno('tabulate by spacing? ','no',status)) then
           option='SPACINGS'
         elseif (io_yesno('tabulate by aerial? ','no',status)) then
           option='AERIALS'
         endif
         list='ALL'
         call get_spacings( ifile, 'spacing list :', ' ', list, ilist,
     :                      max_spac, ns, status                       )
         call chr_chucas(list)
         if (ns.gt.0) then
           call io_opeout(iout,status)
           if (status.eq.0) then
             if (iout.ne.termo) write(iout,*)'PRINT-ZERO-CORRECTIONS'
             write(iout,*)
             call io_lstfil(iout,file,status)
             write(iout,'(/x,a/)')
     :       'Zero-corrections for spacings : '//list(1:chr_lenb(list))
c
c    Report minimum, maximum and mean values
c
             n=0
             sum=0.0
             sumsq=0.0
             zmin=data(1,ilist(1))
             zmax=data(1,ilist(1))
             do is=1,ns
               do i=1,2
                 n=n+1
                 sum=sum+data(i,ilist(is))
                 sumsq=sumsq+data(i,ilist(is))**2
                 if (zmin.gt.data(i,ilist(is))) then
                   zmin=data(i,ilist(is))
                 elseif (zmax.lt.data(i,ilist(is))) then
                   zmax=data(i,ilist(is))
                 endif
               enddo
             enddo
             zmean=sum/n
             zstdev=sqrt((sumsq-sum*sum/n)/n)
             write(iout,1)zmin,zmax,zmean,zstdev
    1        format(' Min,max : ',F6.1,',',F6.1,5X,'mean : ',F5.1,5X,
     *       'standard deviation : ',F6.2/)
c
c    Tabulate by spacing
c
             if (option.eq.'SPACINGS') then
               do is=1,ns
                 rcos(is)=data(1,ilist(is))
                 rsin(is)=data(2,ilist(is))
               enddo
               write(iout,2)
               call table_spac_clfst(ifile,rcos,ilist,ns,1,.true.,
     :                                                           status)
               if (status.eq.0) write(iout,3)
               call table_spac_clfst(ifile,rsin,ilist,ns,1,.true.,
     :                                                           status)
c
c    Tabulate by aerial
c
             elseif (option.eq.'AERIALS') then
               do iae=1,max_aes
                 rcos(iae)=0.0
                 rsin(iae)=0.0
                 nn(iae)=0
               enddo
               do is=1,ns
                 call enq_v1_ae_spac(ilist(is),iae1,iae2,status)
                 rcos(iae1)=rcos(iae1)+data(1,ilist(is))
                 rcos(iae2)=rcos(iae2)+data(1,ilist(is))
                 rsin(iae1)=rsin(iae1)+data(2,ilist(is))
                 rsin(iae2)=rsin(iae2)+data(2,ilist(is))
                 nn(iae1)=nn(iae1)+1
                 nn(iae2)=nn(iae2)+1
               enddo
c
               nae=0
               do iae=1,max_aes
                 if (nn(iae).gt.0) then
                   nae=nae+1
                   ilist(nae)=iae
                   rcos(nae)=rcos(iae)/nn(iae)
                   rsin(nae)=rsin(iae)/nn(iae)
                 endif
               enddo
c
               write(iout,2)
               call table_aes(ifile,rcos,ilist,nae,1,status)
               write(iout,3)
               call table_aes(ifile,rsin,ilist,nae,1,status)
c
             endif
c
    2        format('0Zero-corrections for cosine channel:'/X,36('-'))
    3        format('0Zero-corrections for sine channel:'/X,34('-'))
c
             call io_close(iout,status)
             call io_setout(iold)
             write(iold,*)
c
           endif
         endif
       endif
c
 9999  if (status.ne.0 .and. status .ne. USR_BREAK) then
         call mon_wrerr(status,'in routine PRINT_ZEROS_CLFST')
       endif
       status = 0
c
       call close_sf(ifile,status)
c
c  Update the zero-correction file for subsequent observations
c
       if (io_yesno
     :  ('Do you want to update the zero-correction file?', 'no',
     :                                                     status)) then
         call write_zeros_clfst( file, status )
       endif
c
       write(iold,*)
c
       end
