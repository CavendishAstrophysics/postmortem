C
       integer status
       status=0
       call io_initio
       call io_initlf(.true.)
       call io_setesc(.true.)
       call uv2fits(status)
       call io_setesc(.false.)
       end


*+UV2FITS - Sample data to fits format
*
       subroutine uv2fits(status)

*
*
*  S.Kenderdine   4 August 1988    Version 0.1
*  P.Alexander    12 January 1990  Version 1.0
*                 Modified to new SAMPLIB routines
*                 Will now handle LSF's properly
*                 All access to the sample file is via enquiry routines
*  DJT            28 August 1990  Status handling revised
*                 August 1991 revised for Ryle Telescope
*                 March 1992 revised for output to disc
*                 October 1992 revised to select multiple file output
*                 27 Oct 92 weights derived from rain-gauge readings
*                 December 1992 scaling revised to preserve precision
* GP              19 June 96 - default file-owner for Ryle = temp-user
* DJT             16 Feb 98 - Unix version
*
*-

       integer   status

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/lsflib_errors.inc'
       include '/mrao/post/include/post_common.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'
       include 'uv2fits_common.inc'

       character dirusr*32, name*16, cname*16, type*4

       complex vis(max_vis)
       integer vis_list(max_vis)
       integer baseline(max_vis)
       integer isubb(max_vis),ichan(max_vis)
       integer iaes(max_vis,2)
       integer offset(max_vis)
       real*8 sfreq(max_vis)
       real*8 xyz1(3), xyz2(3)

       integer max_data
       parameter (max_data=6+max_subb*max_channel*3)
       integer*4 data(max_data)
       integer*4 date1,date2

       integer bands(5)
       integer nbands,iband

       integer new_lsf
       integer ndata, num_buff, num_vis
       integer num_ae, num_base, num_sp, num_sb, num_ch
       integer base, subband, channel, tscope
       integer iae, ibuff, isamp, isamp1, isamp2
       integer min_ae, max_ae, min_ich, max_ich
       integer ae1, ae2, spac, subb, chan
       integer ifile, nfiles
       integer i, k, l, i1, i2
       integer sid

       real tsys(max_rt_aes),rg(max_rt_aes),rg_nominal,rg_value
       real uv(2,max_vis), usec, vsec, uv_lim, uv_max
       real*8 weight

       logical rg_weight

       real*8 jd, vfact
       real*8 epoch, ra, dec, skew_angle
       character source*16

       character*1 cband(5)
       data        cband / 'A','B','C','D','E' /

* Entry

       if (status.ne.0) return

       call io_enqvdu(vdu)
       call io_enqout(iodev)
       call io_wrout( ' ' )
       call io_wrout(
     :            'UV2FITS program for writing UV data in FITS format')
       call io_wrout( ' ' )

* Open output device

       call fits_d2t('open',data,0,status)

* Get sample file name

       call getenv('SAMPDIR', def_dir)

 1     uvfile  = ' '
       new_lsf = -1

       call lsf_open(uvfile,new_lsf,'READ',lsf_num,status)
       if (status .eq. 0) then
          call io_wrout ( ' ' )
          call io_wrout ( uvfile )
          call io_wrout ( ' ' )
          call lsf_enq_sf(lsf_num,lun,src_num,status)
          new_lsf = 0
          call lsf_close(lsf_num,status)
          call lsf_open(uvfile,new_lsf,'READ',lsf_num,status)
          if (status .eq. NO_LSFSAVED) then
            status = 0
            new_lsf = -1
            call io_wrout( 'No logical sample files are saved' )
            call lsf_open(uvfile,new_lsf,'READ',lsf_num,status)
          end if
       end if
       if (status.ne.0) goto 999

* Get FITS disc file name

       if (medium.eq.'DISC') then
         call io_wrout(' ')
         call io_brkfil(uvfile,dirusr,name,type)
         i2 = chr_lenb(dirusr)
         i1 = chr_ilstc(dirusr,'/') + 1
         call io_makfil(' ',dirusr(i1:i2),'fits',ffile,l)
         call io_getfil('FITS filename : ','*',ffile,status)
         call io_brkfil(ffile,dirusr,name,type)
         call io_wrout(' ')
       endif
       if (status.ne.0) goto 999


* Read sample file parameters, and list of visibilities

       call enq_clip(lun,aclip,status)
       call enq_phys_tscope(lun,tscope,status)
       call enq_loc_time(lun,idate1,itime1,idate2,itime2,status)
       call enq_obsdef(lun,num_ae,num_sp,num_sb,num_ch,status)
c     write(*,'(4I6)')num_ae,num_sp,num_sb,num_ch
       call lsf_set_spacings(lsf_num,num_vis,vis_list,3,status)
       call lsf_enq_numbuff(lsf_num,num_buff,status)

* Set up index arrays to identify baseline, sub-band and frequency
* channel for each visibility present in the logical sample file.
* Construct baseline identifiers using the aerial numbers, which must
* correspond to the 'antenna numbers' in the antenna extension table.
* Set up array containing channel frequency for each visibility.

       do k=1,num_vis
          call enq_ae_vis(lun,vis_list(k),ae1,ae2,status)
          call enq_vis_desig(lun,vis_list(k),spac,subb,chan,status)
          iaes(k,1)=ae1
          iaes(k,2)=ae2
          if(ae1.lt.ae2) then
            baseline(k)=256*ae1+ae2
          else
            baseline(k)=256*ae2+ae1
          endif
          do i=1,num_sb
            call enq_iba_code(lun,i,subband,status)
            if (subb.eq.subband) isubb(k)=i
          enddo
          do i=1,num_ch
            call enq_ich_code(lun,i,channel,status)
            if (chan.eq.channel) ichan(k)=i
          enddo
          call enq_chfreq(lun,isubb(k),ichan(k),sfreq(k),status)
c     write(*,'(5I6,E14.6)')k,ae1,ae2,isubb(k),ichan(k),sfreq(k)
       enddo

* Identify how many baselines are present in the LSF

       base=0
       num_base=0
       do k=1,num_vis
          if (baseline(k).ne.base) then
             num_base=num_base+1
             base=baseline(k)
          endif
       enddo
cT     write(*,*)num_base,' baselines'

* Identify which sub-bands are present in the LSF

       nsb=0
       nbands=0
       do i=1,num_sb
          do k=1,num_vis
             if (i.eq.isubb(k)) then
                nsb=nsb+1
                nbands=nbands+1
                isb(nsb)=isubb(k)
                bands(nbands)=isubb(k)
                goto 2
             endif
          enddo
 2       continue
       enddo
cT    write(*,'(6I6)')nsb,(isb(i),i=1,nsb)
cT    write(*,'(6I6)')nbands,(bands(i),i=1,nbands)

* Identify the range of channels present in the LSF

       max_ich=0
       min_ich=num_ch
       do k=1,num_vis
          if (ichan(k).lt.min_ich) min_ich=ichan(k)
          if (ichan(k).gt.max_ich) max_ich=ichan(k)
       enddo
       nch = max_ich-min_ich+1
       do i=1,nch
          ich(i)=min_ich+i-1
       enddo
cT    write(*,'(10I6)')nch,(ich(i),i=1,nch)

* Offer multiple output files for multiple sub-bands

       nfiles = 1
       if (nbands.gt.1) then
         if (io_yesno(
     :    'Do you want to write a separate file for each sub-band?',
     :                                               'no', status)) then
           nfiles = nbands
           nsb = 1
         endif
       endif

* For Ryle Telescope, offer rain-gauge visibility weighting

       rg_weight = .false.
       if (tscope.eq.RYLE) then
          if (io_yesno(
     : 'Do you want to write weights derived from rain-gauge readings?',
     :                                                'no',status)) then
            call enq_tsys(lun,tsys,rg_nominal,status)
            do iae=1,max_rt_aes
              tsys(iae)=tsys(iae)/100.0
            enddo
            rg_weight = .true.
          endif
       endif

* Offer sky-projection

       vfact = 1.d0
       if (io_yesno(
     :      'Do you want to adjust U,V values for sky-projection?',
     :                                                'no',status)) then
          call lsf_enq_pc_epoch(lsf_num,epoch,ra,dec,source,status)
          vfact = dsin(dec)
       endif

* Estimate maximum U,V value for scaling purposes

       min_ae=iaes(1,1)
       max_ae=iaes(1,1)
       do k=1,num_vis
         if (iaes(k,1).lt.min_ae) min_ae=iaes(k,1)
         if (iaes(k,2).lt.min_ae) min_ae=iaes(k,2)
         if (iaes(k,1).gt.max_ae) max_ae=iaes(k,1)
         if (iaes(k,2).gt.max_ae) max_ae=iaes(k,2)
       enddo
       call enq_geometry(lun,min_ae,xyz1,status)
       call enq_geometry(lun,max_ae,xyz2,status)
       uv_lim=1.2*sqrt((xyz2(1)-xyz1(1))**2 +
     :                 (xyz2(2)-xyz1(2))**2 +
     :                 (xyz2(3)-xyz1(3))**2 )
       uscal=uv_lim/2.E9

* Set the number of random parameters and the number of groups.
* One group of data is written for each baseline, for each time sample.
* Each group contain visibilities for all sub-bands and frequency
* channels present in the LSF.

       pcount = 6
       gcount = num_base*num_buff
       ndata = (pcount+nsb*nch*3)*2
cT    write(*,'(3I8)')num_base,num_buff,gcount
cT    write(*,'(2I8)')ndata,max_data

* For each visibility, use the sub-band and frequency channel indexes to
* set up the correct offset pointer within the data array

       do k=1,num_vis
          if (nsb.eq.1) then
            offset(k)=pcount+(ichan(k)-min_ich)*3
          else
            do i=1,nsb
               if (isubb(k).eq.isb(i)) then
                  offset(k)=pcount+((i-1)*nch+ichan(k)-min_ich)*3
               endif
            enddo
          endif
cT        write(*,'(8I6)')k,isubb(k),ichan(k),offset(k)
       enddo


*  Loop over multiple output files

       uv_max = 0.0
       skew_angle = 0.d0

       do ifile = 1, nfiles

       if (nfiles.gt.1) then
         iband=ifile
         isb(1)=bands(iband)
         call enq_iba_code(lun,isb(1),subband,status)
         write(iodev,'(X,A,A)')'Subband ',cband(subband)
         if (medium.eq.'DISC') then
           cname=name(1:chr_lenb(name))//'-'//cband(subband)
           call io_makfil(dirusr,cname,'FITS',ffile,l)
           write(iodev,'(2X,A)')ffile(1:l)
         endif
       endif

* Construct FITS header, and write to the output device

       call fits_h2dt(status)

* Read visibility data and write to the output device

       do ibuff=1,num_buff
         call lsf_set_buffer(lsf_num,ibuff,status)
         call lsf_get_vis(lsf_num,max_vis,vis,num_vis,status)
         call lsf_get_uv(lsf_num,max_vis,skew_angle,uv,num_vis,status)
         call lsf_get_sid(lsf_num,sid,status)
         if (status.eq.0) then

           jd=jd0+dfloat(sid)/(864000*const_sut2sst)
           date1=nint((jd-jdazero)/jdascal)
           date2=nint((jd-jdazero-date1*jdascal)/jdbscal)

*     Calculate average rain gauge values for current buffer
           if (rg_weight) then
             do iae = 1,max_rt_aes
               rg(iae)=0.0
             enddo
             call lsf_enq_samples(lsf_num,ibuff,isamp1,isamp2,status)
             do isamp = isamp1,isamp2
               call set_monitor(lun,isamp,status)
               do iae = 1,max_rt_aes
                 call enq_mon_rain(lun,iae,rg_value,status)
                 rg(iae)=rg(iae)+rg_value
               enddo
             enddo
             do iae = 1,max_rt_aes
               rg(iae)=rg(iae)/(isamp2-isamp1+1)
               rg(iae)=rg(iae)/rg_nominal
             enddo
           endif

* Write one group of data for each visibility.  Visibilities are ordered
* by baseline.  The random parameters for each group are 'UU','VV','WW'
* DATE and BASELINE.

           base=0
           do k=1,num_vis
               if (base.ne.baseline(k)) then
                  base=baseline(k)
                  if (k.gt.1) call fits_d2t('write',data,ndata,status)
                  do i=1,ndata/2
                     data(i)=blank
                  enddo
                  usec=uv(1,k)/sfreq(k)
                  vsec= -uv(2,k)*vfact/sfreq(k)
                  uv_max = max(abs(usec),uv_max)
                  uv_max = max(abs(vsec),uv_max)
                  data(1)=usec/uscal
                  data(2)=vsec/vscal
                  data(3)=0
                  data(4)=date1
                  data(5)=date2
                  data(6)=baseline(k)
c      if (abs(usec).gt.uv_lim .or. abs(vsec).gt.uv_lim) then
c         write(*,'(5E12.4)')uv(1,k),uv(2,k),usec,vsec,uv_lim
c      endif
               endif

               weight=1.0
*     Calculate weights derived from rain gauge values.
*     (N.B. this should properly incoporate a weighting reflecting the
*     number of visibilities included by the LSF sampling, but this has
*     not been done here, for consistency with previous processing
*     procedures where these weighting factors have been incorporated
*     within AIPS.  See MEJ for more info.)
               if (rg_weight) weight = rg(iaes(k,1))*rg(iaes(k,2))
     :                                /(tsys(iaes(k,1))*tsys(iaes(k,2)))

               if (nfiles.eq.1 .or. isb(1).eq.isubb(k)) then
                 if (vis(k).eq.(0.0,0.0)) then
                    data(offset(k)+1)=0
                    data(offset(k)+2)=0
                    data(offset(k)+3)=-1
                 else
                    data(offset(k)+1)=nint(real(vis(k))/bscale)
                    data(offset(k)+2)=nint(aimag(vis(k))/bscale)
                    data(offset(k)+3)=nint(weight/bscale)
c     write(*,'(3E16.8,3I12)')vis(k),weight,
c    :              data(offset(k)+1),
c    :              data(offset(k)+2),
c    :              data(offset(k)+3)
                 endif
               endif
           enddo
           call fits_d2t('write',data,ndata,status)

         endif
         if (status.ne.0) goto 999

       enddo

       data(1)=0
       call fits_d2t('fill',data,0,status)

* Antenna table and frequency tables are written as FITS extensions

       call fits_an2dt(status)

       if (nsb.gt.1) then
          call fits_fq2dt(status)
       endif

       call fits_d2t('end',data,0,status)

       enddo

  999  if (status.eq.0) then
         write(iodev,1000) uv_max,int(uv_max/uscal)
 1000    format(/,' Maximum U/V (sec) = ',1PE12.4,I16)
         if (uv_max.gt.uv_lim) then
           write(iodev,*)'*** warning, U/V scaling overflow'
         end if
         write(iodev,*)
       else
         call smp_wrerr(status,'in program UV2FITS')
       end if

       status=0
       call lsf_close(lsf_num,status)

       if (io_yesno(
     :         'Do you want to write more files?','no',status)) goto 1

* Close the output device

       call fits_d2t('close',data,0,status)

       end

