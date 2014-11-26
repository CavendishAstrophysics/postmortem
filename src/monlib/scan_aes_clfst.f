
C+SCAN_AES_CLFST

       subroutine scan_aes_clfst (lsf_num, status)
C      -------------------------------------------
C
C  Executes the SCAN-AERIALS command for the CLFST.
C
C  Given:
C      LSF_NUM       integer       logical sample file number
C      STATUS        integer       status value
C
C  Scans the sample file over a specified range, and finds mean cos,
C  sin, amplitude and phase for all spacings in the spacing list,
C  merged over aerials.  The data can be used to produce a gains file.
C
C  The STATUS value should be zero on entry.
C last mod 12 Apr 2000 GP (io_setacc)
*-
       integer    lsf_num, status

       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/merge_types.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'

       character  title(4)*80
       character  list*80, file*64, gfile*64, string*14
       integer    ibuff1, ibuff2, sf_lun, src_num, tscope
       integer    i, ii, iae, igain, ihut, iold, iout
       integer    nspac, no_group, termi, termo

       integer    no_samp(max_spac)
       integer    sp_list(max_spac),      ilist(2*max_spac)
       integer    group_size(2*max_spac), group_list(2*max_spac)
       real*8     acc(4,max_spac),        acc_sq(4,max_spac)

C  Variables used to calculate scaling factor for output
       integer    j

C  Place work arrays in the work space common block
       common /post/ no_samp, sp_list, ilist, group_size, group_list,
     :               acc, acc_sq

C  Check status on entry
       if (status.ne.0) return

C  Enquire sample file info and output unit
       call lsf_enq_sf( lsf_num, sf_lun, src_num, status )
       call io_enqtio(termi,termo)
       call io_enqout(iold)
       iout=iold

C  Check telescope type
       call enq_phys_tscope( sf_lun, tscope, status )
       if (tscope.ne.CLFST) then
          status = ILL_TSCOPE
       endif

C  Prompt for spacing list and list of spacings to scan
       call get_spacings( sf_lun, 'Scan spacings : ', 'all',
     :                    list, sp_list, max_spac, nspac, status )
       call lsf_set_spacings( lsf_num, nspac, sp_list, 2, status )
       call chr_chucas(list)

C  Define merge list using the specified merge-type
       call set_merge( sf_lun, sp_list, nspac, aerial_merge,
     :                   ilist, no_group, group_size, status )

C  Get range of sample buffers to scan
       call lsf_get_range(lsf_num, ibuff1, ibuff2, status)

C  Prompt for output file
       call io_opeout(iout,status)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'SCAN-AERIALS'
         write(iout,*)
         inquire (unit=sf_lun, name=file)
         call io_lstfil(iout,file,status)
       endif

       if (status.eq.0) then

C  Accumulate data (note the use of ilist as opposed to group_list here)
         call scan_samples(sf_lun,lsf_num,sp_list,nspac,ibuff1,ibuff2,
     :                                      ilist,no_group,group_size,
     :                                        acc,acc_sq,no_samp,status)

C  Compute means and rms deviations
         do i=1,no_group
           if (no_samp(i).gt.0) then
             do j=1,4
               acc(j,i)=acc(j,i)/no_samp(i)
               acc_sq(j,i)=acc_sq(j,i)/no_samp(i)-acc(j,i)*acc(j,i)
               acc_sq(j,i)=dsqrt(dmax1(0.d0,acc_sq(j,i)))
             enddo
           endif
         enddo

C  Initialise and print header text
         call lsf_title(lsf_num,list,ibuff1,ibuff2,title,status)
         write(iout,'(x,a)')title
         write(iout,1)'spacing'
         string = ' '

         ihut = 0
         do i=1, max_aes
           call enq_hut_ae(sf_lun,i,ii,status)
           write(string,'(3X,''aerial'',I3,2X)')i
           if (ii.ne.ihut) write(iout,*)
           ihut=ii

           acc(4,i)=0.0
           acc(3,i)=dsqrt(acc(1,i)**2+acc(2,i)**2)
           if (acc(3,i).gt.0.0)
     :              acc(4,i)=datan2(acc(2,i),acc(1,i))/const_d2r
           write(iout,2)string,acc(1,i),acc_sq(1,i),
     :                         acc(2,i),acc_sq(2,i),acc(3,i),acc(4,i)

           if (io_attn(status)) goto 3
         enddo
C
    1      format(/4X,A,7X,'overall mean cos,',3X,'overall mean sin,',
     :                                      6X,'amp,',5X,'phase')
    2      format(A,5X,2(F7.1,' (',F4.0,')',5X),F7.1,5X,F6.0)

       endif
C
C  Tidy up
    3  call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
C
C
       if (io_yesno(
     :    'Do you want to produce a gains file using this data?',
     :                                               'no', status)) then
         call enq_namfil(sf_lun, 'GAIN', gfile, status)
         if (status.eq.NO_FILE) status=0
         call io_opefil(igain, gfile, 'write', 1, status)
         call io_setacc(gfile, 'r', 'rw', 'rw', status)
         write(igain) (real(acc(3,iae)),iae=1,max_aes)
         write(igain) (real(acc(4,iae)),iae=1,max_aes)
         close(igain)
       else
         write(iold,*)
       endif
C
       if (status.ne.0 .and. status.ne.usr_break) then
         call smp_wrerr(status,'in routine SCAN_AES_CLFST')
         status = 0
       endif

       end
