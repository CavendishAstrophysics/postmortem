*+FLAG_POINT_RYLE

       subroutine flag_point_ryle (file, lsf_key, status)
C      --------------------------------------------------
C
C  Executes the FLAG-POINTING command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      LSF_KEY       integer       current logical sample file key
C      STATUS        integer       status value
C
C  This command scans the sample file and checks aerial pointing in HA and Dec,
C  and prepares flag table entries to exclude gross pointing errors.
C
C  The STATUS value should be zero on entry.
C
C  DJT, 24/4/92; call to enq_off_tables changed 13 Jan 99 GP;
C  more workspace (mrec from 64 to 512) 14 Sep 99 GP; changed error handling
C
*-
       character  file*(*)
       integer    lsf_key, status
c
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/offset_types.inc'
       include '/mrao/post/include/flag_errors.inc'
c
c
       character  list*80
       integer    ilist(max_aes), isamp1, isamp2, nae, num_samp
       integer    ifile, iout, iold, maxc
       integer    istat
c
       character  ffile*64
       character  cstart*4, cstep*4, csamp*4
       real*8     samp_ra, samp_dec
       integer    iha(2), idec(2)
       integer    irec, lrec, nrec, n
       integer    i, iae, isamp, istart, ist1
       integer    l, l1, l2, ls, lsamp, lspac
       integer    lsf_num, sf_lun, src_num
       integer    samp_status, samp_sid_time, samp_wt
       integer    termi, termo
       integer    msamp
       logical    test
c
       integer     ncentre,centre
       character   source(max_centre)*24
       character   sfile(max_centre)*40
       integer     samp_time(max_centre)
       real*8      ra_ref(max_centre),dec_ref(max_centre)
c
       real        offset_angle
       integer     offset_type
       integer     offset_aerial(max_aes)
       integer     offset_time
       integer     ha_5point(0:4), dec_5point(0:4)
       integer     offset2Nx, offset2Ny, offset_zero, offset_row
c
c  Workspace
c
       integer    mrec
       parameter (mrec=512)
       character  string*168
       character  spac_list*16
       character  flag_spac(mrec)*16
       character  flag_samp(mrec)*168
       integer    flag_id, flag_version
       integer*2  iflag(max_samp, max_aes)
       common /post/ flag_spac,flag_samp,iflag
c
c
       if (status.ne.0) return
c
       call io_enqout(iold)
       call io_enqtio(termi,termo)
       iout=iold
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
c
c  Prompt for error threshold (arcsec)
c
       call enq_max_point(ifile,maxc,status)
       call io_geti('flag pointing errors greater than:','*',maxc,
     :                                                        status)
c
c  Offer expected period for offset errors, in the case of multi-centre
c  or offset observations
c
       call enq_centres(ifile,ncentre,source,sfile,samp_time,
     :                           ra_ref,dec_ref,centre,status)
       call enq_off_tables(ifile,offset_type,offset_aerial,
     :                           offset_angle,offset_time,
     :                           ha_5point,dec_5point,
     :                           offset2Nx,offset2Ny,
     :                           offset_zero,offset_row,status)
       if (ncentre.gt.1) then
         msamp=samp_time(centre)
       elseif (offset_type.eq.o_5point .or.
     :         offset_type.eq.o_7point .or.
     :         offset_type.eq.o_raster) then
         msamp=offset_time
       endif
       if (msamp.gt.1) then
         call io_geti('expected period for offset errors:','*',msamp,
     :                                                           status)
       endif
c
c  Prompt for aerial list and sample range
c
       call get_aerials(ifile,'aerial list:','all',list,ilist,
     :                                             max_aes,nae,status)
       call enq_numsamp(ifile,1,num_samp,status)

       isamp1=1
       isamp2=num_samp
       call io_geti('first sample:','*',isamp1,status)
       call io_geti('last sample:','*',isamp2,status)
       isamp1=max(1,isamp1)
       isamp2=min(num_samp,isamp2)
c
c  Prompt for output file
c
       call io_opeout(iout,status)
       if (status.eq.0) then
         if (iout.ne.termo) write(iout,*)'FLAG-POINTING'
         write(iout,*)
         call io_lstfil(iout,file,status)
         call io_setout(iout)
       endif
c
       if (status.eq.0) then
c
         n=0
         do isamp=isamp1,isamp2
           call set_monitor(ifile,isamp,status)
           call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     *                            samp_sid_time,samp_wt,status)
           if (status.eq.0) then
c
c  Read pointing errors for each aerial in the list
c
             do i=1,nae
               iae=ilist(i)
               call enq_aestat(ifile,iae,1,test,status)
               if (test) then
c
                 call enq_mon_hadec(ifile,iae,iha,idec,status)
c
c    Flag pointing errors greater than MAXC
c
                 iflag(isamp,iae)=0
                 if (iha(2).gt.maxc.or.idec(2).gt.maxc) then
                   iflag(isamp,iae)=1
                 endif
               endif
             enddo
             n=n+1
c
           endif
         enddo
c
         isamp2=isamp1+n-1
         write(iout,1)n,isamp1,isamp2
    1    format(I5,' samples read,',I5,' to',I5/)
c
       endif
c
       istat=0
       call close_sf(ifile,istat)
c
c    Construct flag table entries from flagged values
c
       if (status.eq.0) then
         nrec=0
         lrec=len(flag_samp(1))
         do i=1,nae
           l=0
           iae=ilist(i)
           spac_list='ae'
           call chr_chitoc(iae,spac_list(4:),ls)
c
c    Look for periodic errors occurring every MSAMP samples.  These are
c    only recognised if present throughout the entire sample range.
c
           if (msamp.gt.1 .and. msamp.lt.n) then
             ist1=isamp1
C    ... treat first sample as special case when looking for offset errors
             if (ist1.eq.1 .and. iflag(1,iae).eq.0) ist1=2
             do istart=ist1,ist1+msamp-1
               isamp=istart
               do while (isamp.le.isamp2)
                 if (iflag(isamp,iae).eq.0) goto 2
                 isamp=isamp+msamp
               enddo
c    ... periodic error found, update flags by setting to -1
               do isamp=istart,isamp2,msamp
                 iflag(isamp,iae)= -1
               enddo
               call chr_chitoc(istart,cstart,l1)
               call chr_chitoc(msamp,cstep,ls)
               call chr_chitoc(isamp2,csamp,l2)
               if (l+l1+ls+l2+3.gt.lrec) then
                 if (nrec.lt.mrec) then
                   nrec = nrec + 1
                   flag_spac(nrec) = spac_list
                   flag_samp(nrec) = string(2:l)
                   l=0
                 else
                   write(iout,*) '*** too many flag table entries'
                   write(iout,*) '*** flag table not updated'
                   goto 5
                 endif
               endif
               string(l+1:l+l1+ls+l2+3) = ','//cstart(1:l1)//
     :                                '('//cstep(1:ls)//')'//csamp(1:l2)
               l=l+l1+ls+l2+3
    2          continue
             enddo
           endif
c
c  Find remaining isolated errors and runs
c
           isamp=isamp1
    3      continue

           do while (isamp.le.isamp2 .and. l+ls+1.le.lrec)
             if (iflag(isamp,iae).eq.1) then
               call chr_chitoc(isamp,csamp,ls)
               string(l+1:l+ls+1)=','//csamp(1:ls)
               isamp=isamp+1
               l=l+ls+1
               l1=l
               if (l+ls+1.le.lrec) then
                 do while (isamp.le.isamp2 .and. iflag(isamp,iae).ne.0)
                   call chr_chitoc(isamp,csamp,ls)
                   string(l+1:l+ls+1)='-'//csamp(1:ls)
                   isamp=isamp+1
                   l1=l+ls+1
                 enddo
               else
                 isamp=isamp-1
               endif
               l=l1
             endif
             isamp=isamp+1
           enddo

           if (l.gt.0) then
             if (nrec.lt.mrec) then
               nrec = nrec + 1
               flag_spac(nrec) = spac_list
               flag_samp(nrec) = string(2:l)
               l=0
               goto 3
             else
               write(iout,*) '*** too many flag table entries'
               goto 4
             endif
           endif

         enddo
c
c    Report flag table entries
c
    4    do irec=1,nrec
           lspac=chr_lenb(flag_spac(irec))
           lsamp=chr_lenb(flag_samp(irec))
           write(iout,*)flag_spac(irec)(1:lspac)//' % '//
     :                                          flag_samp(irec)(1:lsamp)
         enddo
         write(iout,*)
       endif
c
c    Apply flag table entries to the flag file
c
       if (nrec.gt.0) then
         call lsf_open(file,lsf_key,'READ',lsf_num,status)
         call lsf_enq_sf(lsf_num,sf_lun,src_num,status)
         if (io_yesno(
     :     'Do you want to add these entries to the flag file?',
     :                                                'no',status)) then
           call enq_namfil(sf_lun,'FLAG',ffile,status)
           call flag_open(ffile,flag_id,status)
           if (status.eq.0) then
             flag_version=0
             do irec=1,nrec
               lspac=chr_lenb(flag_spac(irec))
               lsamp=chr_lenb(flag_samp(irec))
               call flag_write_entry(flag_id,lsf_num,sf_lun,
     :                               flag_version,'POSTMORTEM',
     :                               flag_spac(irec)(1:lspac),
     :                               flag_samp(irec)(1:lsamp),
     :                               'set','pointing errors',status)
             enddo
             call flag_close(flag_id,status)
           endif
         endif
c
         if (io_yesno('Do you want to add more entries interactively?',
     :                                                'no',status)) then
           call enq_namfil(sf_lun,'FLAG',ffile,status)
           call flag_open(ffile,flag_id,status)
           do while (status.eq.0)
             flag_version=0
             spac_list=' '
             call io_getstr('aerial list:',' ',spac_list,status)
             if (spac_list.eq.' ') status=USR_BREAK
             call io_getstr('sample list:',' ',flag_samp(1),status)
             flag_spac(1)='ae '//spac_list
             lspac=chr_lenb(flag_spac(1))
             lsamp=chr_lenb(flag_samp(1))
             call flag_write_entry(flag_id,lsf_num,sf_lun,
     :                             flag_version,'POSTMORTEM',
     :                             flag_spac(1)(1:lspac),
     :                             flag_samp(1)(1:lsamp),
     :                             'set','pointing errors',status)
             if (status.eq.ILL_FLGDATA) status=0
           enddo
           if (status.eq.USR_BREAK) status=0
           call flag_close(flag_id,status)
c
         endif
         call lsf_close(lsf_num,status)
       endif
c
    5  if (status.eq.USR_BREAK) then
         status=0
       else if (status.ne.0) then
         call mon_wrerr(status,'in routine FLAG_POINT_RYLE')
       endif
c
       call io_close(iout,status)
       call io_setout(iold)
       write(iold,*)
c
       end


