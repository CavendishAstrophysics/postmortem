

*+LIST_SPAC_CLFST

       subroutine list_spac_clfst (file, status)
C      -----------------------------------------
C
C  Executes the LIST-SPACINGS command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to list or tabulate spacings present within a given
C  sample file, in terms of spacing number and aerial pairs.
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
       character  list*80, option*5
       real       avsp
       integer    buffer(2,5)
       integer    i, ii, is, isp
       integer    ifile, iout, iold, termi, termo
       integer    chr_lenb, ns, minsp, maxsp
       logical    means
c
c  Workspace
c
       real       alist(max_spac)
       integer    ilist(max_spac)
       common /post/ alist, ilist
c
c
       if (status.ne.0) return
c
       call io_enqtio(termi,termo)
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
c
c  Prompt for further parameters and options
c
       option=' '
       if (io_yesno(
     :            'list in order of spacing number? ','no',status)) then
         option='LIST'
       elseif (io_yesno('tabulate by aerial number? ','no',status)) then
         means=io_yesno(
     :               '... including row and column means? ','no',status)
         option='TABLE'
       endif
       list='all'
       call get_spacings(ifile, 'spacing list :', ' ', list, ilist,
     :                                             max_spac, ns, status)
       call chr_chucas(list)
       if (ns.gt.0) then
         call io_enqout(iold)
         call io_opeout(iout,status)
         if (status.eq.0) then
           if (iout.ne.termo) write(iout,*)'LIST-SPACINGS'
           write(iout,*)
           call io_lstfil(iout,file,status)
           write(iout,'(X,A/)')'Spacings : '//list(1:chr_lenb(list))
         endif
       endif
c
       if (ns.gt.0 .and. status.eq.0) then
c
c    List by spacing number
c
         if (option.eq.'LIST') then
           ii=0
           do is=1,ns
             ii=ii+1
             call enq_spac_no(ifile,ilist(is),alist(ii),status)
             buffer(1,ii)=ispae(1,ilist(is))
             buffer(2,ii)=ispae(2,ilist(is))
             if (ii.eq.5 .or. is.eq.ns) then
               write(iout,1)(alist(i),buffer(1,i),buffer(2,i),i=1,ii)
    1          format(5(F7.1,':',I3,',',I2,1X))
               if (io_attn(status)) goto 2
               ii=0
             endif
           enddo
    2      write(iout,*)
c
c    Tabulate by aerial
c
         elseif (option.eq.'TABLE') then
           do is=1,ns
             isp=ilist(is)
             alist(is)=(x(ispae(2,isp))-x(ispae(1,isp)))*spfac
           enddo
           call table_spac_clfst(ifile,alist,ilist,ns,1,means,status)
c
         endif
c
         call mean_spac(ifile,ilist,ns,minsp,maxsp,avsp,status)
         write(iout,3)ns,minsp,maxsp,avsp
    3    format(I4,' spacings,  minimum',I4,'  maximum',I4,
     :                                      ',  mean spacing',F7.1)
c
       endif
c
       if (status.ne.0 .and. status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine LIST_SPAC_CLFST')
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
