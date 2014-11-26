
*+SCAN_SF_CLFST

       subroutine scan_sf_clfst (file, status)
C      ---------------------------------------
C
C  Executes the SCAN-SAMPLE-FILE command.
C
C  Given:
C      FILE          char*(*)      sample file name
C      STATUS        integer       status value
C
C  This commands scans the sample file over a specified range of samples,
C  and prints out the following information for each aerial:
C
C  - mean pointing errors in units of 3 mins(HA) and 3/8deg(Dec).
C  - mean AGC levels, with rms deviations.
C  - mean amplitude and phase, calculated by merging cos and sin over
C    the sample range;  together with rms deviations from the average
C    amplitudes and phases, merged over the sample range.
C
C  The STATUS value should be zero on entry.
C
*-
       character  file*(*)
       integer    status
c
       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/merge_types.inc'
       include '/mrao/post/include/mon_v1_block.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'
c
       character  header(4)*80
       character  text*80, list*80
       real*8     ra, dec, epoch
       real*4     rcos, rsin, ramp, rphi
       integer    ifile, iold, iout, isamp, isamp1, isamp2
       integer    i, iae, iae1, iae2, ihut, istep, chr_lenb
       integer    n, nae, nspac, num_samp
       integer    termi, termo
c
c  Workspace arrays
c
       integer    mspac(max_aes)
       integer    sp_list(max_spac)
       integer    ilist(2*max_spac)
       complex    vis(max_spac)
       complex    vis_ae(max_aes)
       real*8     acc(8,max_aes)
       real*8     acc_sq(8,max_aes)

       common /post/ vis, vis_ae, acc, acc_sq, mspac, sp_list, ilist
c
       if (status.ne.0) return
c
       call io_enqtio(termi,termo)
       call io_enqout(iold)
       iout=iold
c
c  Open the sample file and read control tables
c
       call open_sf(ifile,file,'read',0,status)
       call open_source(ifile,1,0,status)
c
       if (status.eq.0) then
c
c    Prompt for spacing list and sample range
c
         call get_spacings(ifile, 'spacing list :', 'all', list,
     :                                 sp_list, max_spac, nspac, status)
         call set_merge(ifile, sp_list, nspac, aerial_merge,
     :                                           ilist,nae,mspac,status)
         call enq_numsamp(ifile,1,num_samp,status)
         isamp1=1
         isamp2=num_samp
         call io_geti('first sample:','*',isamp1,status)
         call io_geti('last sample:','*',isamp2,status)
         isamp1=max(1,isamp1)
         isamp2=min(num_samp,isamp2)
         call chr_chucas(list)
c
c    Prompt for output file
c
         call io_opeout(iout,status)
         if (status.eq.0) then
           if (iout.ne.termo) write(iout,*)'SCAN-SAMPLE-FILE'
           write(iout,*)
           call io_lstfil(iout,file,status)
         endif
c
       endif
c
c
       if (status.eq.0) then
c
c  Initialise and print header text
c
         istep=1
         call enq_pc_epoch(ifile,1,epoch,ra,dec,text,status)
         text='Spacings: '//list(1:chr_lenb(list))
         call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
         header(3)(1:12)='Map centre'
         write(iout,'(X,A)')header
c
c  Initialise working arrays for statistics
c
         do iae=1,max_aes
           do i=1,8
             acc(i,iae)=0.d0
             acc_sq(i,iae)=0.d0
           enddo
         enddo
c
c  Scan the sample file
c
         n=0
         do isamp=isamp1,isamp2
           call read_monitor(ifile,isamp,mon_length,mon_block,status)
           call read_sample(ifile,1,isamp,1,nspac,sp_list,vis,status)
           call merge_vis_buffer(vis,nspac,ilist,nae,mspac,vis_ae,
     :                                                        status)
           if (status.eq.0) then
c
c  For each aerial in the list, accumulate AGC's and pointing errors
c  together with cos, sine, amplitude and phase merged over aerials.
c
             do iae=1,max_aes
               rphi=0.0
               rcos=real(vis_ae(iae))
               rsin=aimag(vis_ae(iae))
               ramp=cabs(vis_ae(iae))
               if (ramp.gt.0.0) rphi=atan2(rsin,rcos)/const_d2r
               acc(1,iae)=acc(1,iae)+rcos
               acc(2,iae)=acc(2,iae)+rsin
               acc(3,iae)=acc(3,iae)+ramp
               acc(4,iae)=acc(4,iae)+rphi
               acc(5,iae)=acc(5,iae)+mon_agc(iae)
               acc(6,iae)=acc(6,iae)+(mon_ha(iae,2)-mon_ha(iae,1))
               acc(7,iae)=acc(7,iae)+(mon_dec(iae,2)-mon_dec(iae,1))
               acc_sq(3,iae)=acc_sq(3,iae)+ramp*ramp
               acc_sq(4,iae)=acc_sq(4,iae)+rphi*rphi
               acc_sq(5,iae)=acc_sq(5,iae)+mon_agc(iae)*mon_agc(iae)
             enddo
             n=n+1
c
           endif
         enddo
       endif
c
c
       if (n.gt.0 .and. status.eq.0) then
c
         do iae=1,max_aes
c
c    Compute means
c
           do i=1,7
             acc(i,iae)=acc(i,iae)/n
           enddo
c
c    Compute rms deviations for AGCs and overall amplitude and phase
c
           do i=3,5
             acc_sq(i,iae)=acc_sq(i,iae)/n-acc(i,iae)*acc(i,iae)
             acc_sq(i,iae)=dsqrt(dmax1(0.d0,acc_sq(i,iae)))
           enddo
c
c    Compute amplitude and phase from overall mean cos and sine
c
           acc(4,iae)=0.d0
           acc(3,iae)=dsqrt(acc(1,iae)**2+acc(2,iae)**2)
           if (acc(3,iae).gt.0.d0)
     :       acc(4,iae)=datan2(acc(2,iae),acc(1,iae))/const_d2r
c
         enddo
c
         write(iout,1)
c
c    Print out statistics by hut
c
         ihut=1
         do while (ihut.le.max_huts .and. status.eq.0)
           call enq_ae_hut(ifile,ihut,iae1,iae2,status)
           write(iout,*)
           do iae=iae1,iae2
             write(iout,2)iae,acc(6,iae),acc(7,iae),
     :                        acc(5,iae),acc_sq(5,iae),
     :                        acc(3,iae),acc_sq(3,iae),
     :                        acc(4,iae),acc_sq(4,iae)
           enddo
           if (io_attn(status)) goto 3
           ihut=ihut+1
         enddo
         write(iout,*)
c
    1    format(/8X,'pointing errors',7X,'AGC levels',
     :           9X,'amplitude',12X,'phase')
    2    format(2X,'ae',I3,2F8.1,3X,F7.1,' (',F4.1,')',4X,F7.1,
     :             ' (',F4.0,')',4X,F6.0,' (',F4.0,')')
c
       endif
c
    3  if (status.ne.0.and.status.ne.USR_BREAK) then
         call mon_wrerr(status,'in routine SCAN_SF_CLFST')
       endif
       status=0
c
       call close_sf(ifile,status)
c
       call io_close(iout,status)
       call io_setout(iold)
c
       end
