
C+scan_sample_clfst

       subroutine scan_sample_clfst (lsf_num, status)
C      ----------------------------------------------
C
C  Executes the SCAN-SAMPLE-FILE command for the CLFST.
C
C  Given:
C      LSF_NUM       integer       logical sample file number
C      STATUS        integer       status value
C
C  Scans the sample file over a specified range, and prints means of cos,
C  sin, amplitude and phase for all spacings in the spacing list.
C  The merge type determines the type of scan performed.
C
C  Merge types currently supported:
C     no_merge
C     aerial_merge
C     hut_sw_merge
C     hut_merge
C     total_merge
C
C  The STATUS value should be zero on entry.
C
*-
       integer    lsf_num, merge_type, status

       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/merge_types.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'

       character  title(4)*80, hut_ident(8)*1
       character  list*80, file*64, string*14, option*5
       integer    ibuff1, ibuff2, sf_lun, src_num, tscope
       integer    i, ii, iae, iae1, iae2, ihut, iold, iout, isp
       integer    nae, nspac, no_group, merge_ctrl
       integer    termi, termo
       real       ramp, rphi
       logical    tabcos, tabamp, tabphi, tabrms
       logical    scan_cs, scan_aphi, print

       integer    ae_list(max_aes),       sp_list(max_spac)
       integer    no_samp(max_spac),      ilist(2*max_spac)
       integer    group_size(2*max_spac), group_list(2*max_spac)
       real*8     acc(4,max_spac),        acc_sq(4,max_spac)
       real*4     values(max_spac),       rms(max_spac)

C  Variables used to calculate scaling factor for output
       real*8     acc_max, scale
       integer    j, j1, j2, i_power

C  Define values for hut identifiers
       data       hut_ident / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H' /

C  Place work arrays in the work space common block
       common  /post/  ae_list, sp_list, no_samp, ilist,
     :                 group_size,  group_list,
     :                 acc, acc_sq, values, rms

C  Check status on entry
       if (status.ne.0) return

C  Enquire sample file info and output unit
       call lsf_enq_sf( lsf_num, sf_lun, src_num, status )
       call io_enqout(iold)
       call io_enqtio(termi,termo)
       iout=iold

C  Check telescope type
       call enq_phys_tscope( sf_lun, tscope, status )
       if (tscope.ne.CLFST) then
          status = ILL_TSCOPE
       endif

C  Prompt for merge type
    1  call get_merge( sf_lun, 'Specify merge type:', 'none',
     :                                               merge_type, status)
       if (merge_type.eq.west_aerials .or.
     :     merge_type.eq.east_aerials) then
         call smp_wrerr(ILL_MERGE,' ')
         goto 1
       endif

       if (status.eq.0) then

C  Prompt for analysis options
         option=' '
         scan_cs=io_yesno('Do you want an average over cos and sin? ',
     :                                                     'yes',status)
         if (.not.scan_cs) then
            write(iout,*)'... average over amplitude and phase'
         endif
         scan_aphi=.not.scan_cs

C  Prompt for display options, listing or cross-tabulation
         merge_ctrl = merge_type
         if (merge_ctrl.eq.no_merge) then
            if (io_yesno('Do you want a list in order of spacing? ',
     :                                               'no', status)) then
               option='LIST'
            endif
         elseif (merge_ctrl.eq.aerial_merge) then
           if (io_yesno('Do you want a list by aerial number? ',
     :                                               'no', status)) then
              option='LIST'
           endif
         elseif (merge_ctrl.eq.hut_merge) then
           option='LIST'
         elseif (merge_ctrl.eq.hut_sw_merge) then
           option='TABLE'
         elseif (merge_ctrl.eq.total_merge) then
           option='LIST'
         endif

         if (option.ne.'LIST') then
           option='TABLE'
           tabcos=.false.
           tabamp=.false.
           tabphi=.false.
           tabrms=.false.
           if (scan_cs) then
             tabcos=io_yesno('Tabulate overall mean cos and sin? ',
     :                                                      'no',status)
             if (tabcos) then
               tabrms=io_yesno('... with rms errors?','no',status)
             else
               tabamp=io_yesno('Tabulate amplitude? ','no',status)
               tabphi=io_yesno('Tabulate phase? ','no',status)
             endif
           elseif (scan_aphi) then
             tabamp=io_yesno('Tabulate overall mean amplitude? ','no',
     :                                                           status)
             tabphi=io_yesno('Tabulate overall mean phase? ','no',
     :                                                           status)
             if (tabamp .or. tabphi) then
               tabrms=io_yesno('... with rms errors?','no',status)
             endif
           endif
         endif

C  Prompt for spacing list and list of spacings to scan
         call get_spacings( sf_lun, 'Scan spacings : ', 'all',
     :                      list, sp_list, max_spac, nspac, status )
         call lsf_set_spacings( lsf_num, nspac, sp_list, 2, status )
         call chr_chucas(list)

C  Define merge list using the specified merge-type
         call set_merge( sf_lun, sp_list, nspac, merge_ctrl,
     :                   ilist, no_group, group_size, status )

C  Prompt for list of aerials to display
         if (merge_ctrl .eq. aerial_merge) then
           call get_aerials(sf_lun, 'Display aerials:', 'all',
     :                           string, ae_list, max_aes, nae, status )
         endif

C  Get range of sample buffers to scan
         call lsf_get_range(lsf_num, ibuff1, ibuff2, status)

C  Prompt for output file
         call io_opeout(iout,status)
         if (status.eq.0) then
           if (iout.ne.termo) write(iout,*)'SCAN-SAMPLE-FILE'
           write(iout,*)
           inquire (unit=sf_lun, name=file)
           call io_lstfil(iout,file,status)
         endif
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

C  Set up scaling for data output
         i_power = 100
         acc_max = 0.0D+0
         if (scan_cs) then
           j1 = 1
           j2 = 2
         else
           j1 = 3
           j2 = 3
         endif
         do i=1,no_group
           do j=j1,j2
             acc_max = max(acc_max,abs(acc(j,i)))
             if (acc_max.gt.0.d0) then
               i_power = min(i_power,int(log10(99999.0/acc_max)))
               acc_max = max(acc_max,abs(acc_sq(j,i)))
               i_power = min(i_power,int(log10(999.0/acc_max)))
             endif
           enddo
         enddo
C  Enforce scale factor 1.0 for CLFST
         i_power = 0
         if (i_power.ne.0) then
           scale = 10.0**i_power
           call io_getd('Scale factor for output :', '*', scale, status)
           if (iout.gt.1) then
             write(iout,'(1X,A,1PD10.1)') 'Data scaled : ',scale
           endif
           write(iout,*)
           do i=1,no_group
             do j=j1,j2
               acc(j,i) = scale*acc(j,i)
               acc_sq(j,i) = scale*acc_sq(j,i)
             enddo
           enddo
         endif
       endif

C-----------------------------------------------------------------------
C Report the statistics
C =====================
C
       if (status.eq.0) then

C  Initialise and print header text
         call lsf_title(lsf_num,list,ibuff1,ibuff2,title,status)
         write(iout,'(x,a)')title
C
         if (option.eq.'LIST') then
           string = ' '
           if (merge_ctrl.eq.no_merge)string = 'spacing'
           if (scan_cs) write(iout,2)string
           if (scan_aphi) write(iout,3)string
           ihut = 0

           do i=1,no_group

             string = ' '
             print=.true.
             if (merge_ctrl.eq.no_merge) then
                 call enq_isp_code(sf_lun,sp_list(i),isp,status)
                 call enq_ae_spac(sf_lun,sp_list(i),iae1,iae2,status)
                 write(string,'(I4,2X,''ae'',I3,'','',I2)')
     :                                                     isp,iae1,iae2
                 if (i.eq.1) write(iout,*)
             elseif (merge_ctrl.eq.aerial_merge) then
                 print=.false.
                 do iae=1,nae
                   if (i.eq.ae_list(iae)) then
                     print=.true.
                     call enq_hut_ae(sf_lun,i,ii,status)
                     write(string,'(3X,''aerial'',I3,2X)')i
                     if (ii.ne.ihut) write(iout,*)
                     ihut=ii
                   endif
                 enddo
             elseif (merge_ctrl.eq.hut_merge) then
                 write(string,'(4X,''hut '',A,5X)')hut_ident(i)
                 if (i.eq.1) write(iout,*)
             elseif (merge_ctrl.eq.total_merge) then
                 string = '  Total'
             endif

             if (print) then
               if (scan_cs) then
                 rphi=0.0
                 ramp=dsqrt(acc(1,i)**2+acc(2,i)**2)
                 if (ramp.gt.0.0)
     :              rphi=datan2(acc(2,i),acc(1,i))/const_d2r
                 write(iout,4)string,acc(1,i),acc_sq(1,i),
     :                               acc(2,i),acc_sq(2,i),ramp,rphi

               elseif (scan_aphi) then
                 write(iout,4)string,acc(3,i),acc_sq(3,i),
     :                               acc(4,i),acc_sq(4,i)

               endif
             endif
             if (io_attn(status)) goto 5
           enddo
C
    2      format(/4X,A,'overall mean cos,',3X,'overall mean sin,',
     :                                      6X,'amp,',5X,'phase')
    3      format(/4X,A,'overall mean amp,',3X,'overall mean phase')
    4      format(A,5X,2(F7.1,' (',F4.0,')',5X),F7.1,5X,F6.0)


C  Tabulation
C  ==========

         elseif (option.eq.'TABLE') then

C  Define the group_list array. This passes information to the table_...
C  routines defining the type of data they are to tabulate.
         if (merge_ctrl.eq.no_merge) then
           no_group = nspac
           do i = 1,no_group
             group_list(i) = sp_list(ilist(i))
           enddo
         elseif (merge_ctrl.eq.aerial_merge) then
           no_group = nae
           do i = 1,no_group
             group_list(i) = ae_list(i)
             do ii = 1,4
               acc(ii,i) = acc(ii,group_list(i))
               acc_sq(ii,i) = acc_sq(ii,group_list(i))
             enddo
           enddo
         endif

C  Report overall mean cos and sine
           write(iout,*)
           if (scan_cs) then
             if (tabcos) then
               do i=1,no_group
                 values(i)=acc(1,i)
                 rms(i)=acc_sq(1,i)
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                   no_group,1,.true.,'Overall mean cosine',status)
               if (tabrms) then
                 call table_data(sf_lun,merge_ctrl,rms,group_list,
     :                   no_group,0,.false.,'... rms deviation',status)
               endif

               do i=1,no_group
                 values(i)=acc(2,i)
                 rms(i)=acc_sq(2,i)
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                   no_group,1,.true.,'Overall mean sine',status)
               if (tabrms) then
                 call table_data(sf_lun,merge_ctrl,rms,group_list,
     :                   no_group,0,.false.,'... rms deviation',status)
               endif

             endif

             if (tabamp) then
               do i=1,no_group
                 values(i)=dsqrt(acc(1,i)**2+acc(2,i)**2)
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                   no_group,1,.true.,
     :                 'Amplitude from overall mean cos and sin',status)
             endif

             if (tabphi) then
               do i=1,no_group
                 values(i)=0.0
                 if (acc(1,i).ne.0.d0 .or. acc(2,i).ne.0.d0)
     :             values(i)=datan2(acc(2,i),acc(1,i))/const_d2r
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                   no_group,1,.true.,
     :                 'Phase from overall mean cos and sin',status)
             endif
           endif

C  Report overall mean amplitude and phase
           if (scan_aphi) then
             if (tabamp) then
               do i=1,no_group
                 values(i)=acc(3,i)
                 rms(i)=acc_sq(3,i)
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                no_group,1,.true.,'Overall mean amplitude',status)
               if (tabrms) then
                 call table_data(sf_lun,merge_ctrl,rms,group_list,
     :                no_group,0,.false.,'... rms deviation',status)
               endif
             endif

             if (tabphi) then
               do i=1,no_group
                 values(i)=acc(4,i)
                 rms(i)=acc_sq(4,i)
               enddo
               call table_data(sf_lun,merge_ctrl,values,group_list,
     :                no_group,1,.true.,'Overall mean phase',status)
               if (tabrms) then
                 call table_data(sf_lun,merge_ctrl,rms,group_list,
     :                no_group,0,.false.,'... rms deviation',status)
               endif
             endif
           endif

         endif
       endif

C  Tidy up
    5  call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)

       if (status.ne.0 .and. status.ne.usr_break) then
         call smp_wrerr(status,'in routine SCAN_SAMPLE_CLFST')
       endif

       end
