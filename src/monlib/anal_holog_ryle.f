C+ANAL_HOLOG_RYLE

       subroutine anal_holog_ryle (lsf_num, plot_device, status)
C      ---------------------------------------------------------
C
C  Analyse holography raster observations.
C
C  Given:
C      LSF_NUM       integer       logical sample file number
C      PLOT_DEVICE   char*(*)      PGPLOT device
C      STATUS        integer       status value
C
C  Scans the given logical sample file, for a raster observation
C  on a point source, with one reference aerial not offset, merging
C  visibilities for each spacing (i.e. over sub-band/channel), and
C  producing data files for subsequent analysis.
C
C  The STATUS value should be zero on entry.
C
C  21 October 92; call to enq_offset_tables changed 13 Jan 99
C
*-

       integer        lsf_num, status
       character*(*)  plot_device

      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/phys_tscopes.inc'
      include '/mrao/post/include/merge_types.inc'
      include '/mrao/post/include/offset_types.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/constants.inc'

       character  title(4)*80
       character  list*80, source*80, text*8
       character*64  file

       integer    ibuff, ibuff1, ibuff2, sf_lun, src_num, tscope
       integer    i, j, iae1, iae2, isp, iba, ich
       integer    iold, iout, termi, termo
       integer    n

       integer    merge_type
       integer    aerial, aerial1, aerial2
       integer    nsize, nspac, no_groups
       real       offset_angle
       integer    offset_type, offset_time
       integer    offset_aerial(8)
       integer    offset2Nx, offset2Ny, offset_zero, offset_row
       integer    ha_5point(0:4), dec_5point(0:4)
       integer    sm_type, sm_size, sm_rate, sid
       integer    integ, offsetx, offsety
       real*8     epoch, radate, decdate, freq
       real*8     delay, dummy, elev, ha

       integer    sp_list(max_vis)
       integer    merge_list(2*max_vis), group_size(2*max_vis)

       complex    vis_list(max_vis), vis_merge(max_vis)

       integer    haoff(max_samp), decoff(max_samp)
       real       hour_angle(max_samp)
       real       amp(max_samp),  phi(max_samp)

       logical    Lprint

       character  sub_bands(5)*1
       data       sub_bands /'A','B','C','D','E'/

       real       pi, min_amp
       parameter (pi = const_pi, min_amp = 0.05)

C  Place work arrays in the work space common block
       common  /post/  sp_list, merge_list, group_size,
     :                 vis_list, vis_merge,
     :                 hour_angle, amp, phi,
     :                 haoff, decoff


       if (status.ne.0) return

       call io_enqtio(termi,termo)
       call io_enqout(iout)
       iold = iout

C  Sample file info
       call lsf_enq_sf(lsf_num, sf_lun, src_num, status)

C  Check telescope type
       call enq_phys_tscope(sf_lun, tscope, status)
       if (tscope.ne.RYLE) status = ILL_TSCOPE

       call enq_pc_epoch(sf_lun,1,epoch,radate,decdate,source,status)
       call enq_freq(sf_lun, freq, status)

C  Set merge type:
C   'subband' merges visibilities over channels and sub-bands for each
C      spacing, providing one output visibility for each spacing.
C   'channel' merges visibilities over channels for each sub-band,
C      providing one output visibility for each sub-band/spacing.

*      merge_type = subband_merge
       merge_type = channel_merge

C  Check offset type, report aerials

       call enq_off_tables (sf_lun, offset_type, offset_aerial,
     :                      offset_angle, offset_time,
     :                      ha_5point, dec_5point,
     :                      offset2Nx, offset2Ny,
     :                      offset_zero, offset_row, status)

       if (offset_type .ne. o_raster) then
          write (iout,*) '*** not a raster observation'
          goto 999
       elseif (offset2Nx.ne.offset2Ny) then
          write (iout,*) '*** raster not square'
       else
          nsize = 2*offset2Nx + 1
       endif

       text = '        '
       do j = 1,8
          if (offset_aerial(j) .eq. 1)
     :            text(j:j) = char(ichar('0')+j)
       enddo

       write (iout, *) ' aerials offset are: ', text
       write (iout, 1030) ' offset angle', offset_angle,' min arc; ',
     :                   offset_time, ' samples/point'
 1030  format(1x, A, f5.1, A, i3, A)

C  Check sampling

       call lsf_enq_smooth (lsf_num, sm_type, sm_size, sm_rate, status)
       write (iout, 1002) sm_type, sm_size, sm_rate
 1002  format(1x, 'smooth type',i2,'  length',i3,'  sample-rate',i3)

       if (offset_time .ne. sm_size  .or.
     :     offset_time .ne. sm_rate) then
          write (iout,'(x,a,i4)')
     :          '*** set smoothing and sampling to',offset_time
          goto 999
       endif

C  Get effective integration time
       call lsf_enq_integ(lsf_num, integ, status)

C  Get range of sample buffers to scan
       call lsf_get_range(lsf_num, ibuff1, ibuff2, status)
       if (status.eq.0) then
          if (ibuff2-ibuff1 .lt. nsize*(nsize+1)) then
             write (iout,*) '*** less than complete raster'
             goto 999
          else
             ibuff2 = ibuff1 + nsize*(nsize+1)
          endif
       endif

       lprint = .true.
c      lprint = io_yesno('print the raw data ?', 'no', status)

C  Output file
       call io_opeout(iout, status)
       if (status.eq.0) then
         if (iout.ne.termo)  write(iout,*) 'Holography analysis'
         write(iout,*)
         inquire (unit=sf_lun, name=file)
         call io_lstfil(iout, file, status)
       endif

       call lsf_title(lsf_num, list, ibuff1, ibuff2, title, status)
       write(iout,'(x,a)')title
       write(iout,*)

C  List of spacings to scan.  This should specify an aerial pair involving
C  one offset aerial, and a single sub-band.

 500   call get_spacings(sf_lun,
     :        'Spacings (specify aerial pair and subband) : ', ' ',
     :                        list, sp_list, max_vis, nspac, status)
       call lsf_set_spacings(lsf_num, nspac, sp_list, 2, status)
       call chr_chucas(list)

       call lsf_enq_ae_vis(lsf_num, 1, aerial1, aerial2, status)
c      write (iold, '(1x, A, 2i2)') 'aerials', aerial1, aerial2
       do  i = 1, nspac
          call lsf_enq_ae_vis(lsf_num, i, iae1, iae2, status)
          if (aerial1 .ne. iae1 .or. aerial2 .ne. iae2) then
             write (iold, *) '*** more than one aerial pair included'
             goto 500
          endif
       enddo

C  Make sure that only one aerial is offset

       if (offset_aerial(aerial1) .eq. 1
     :       .and. offset_aerial(aerial2) .eq. 0) then
          aerial = aerial1
       elseif (offset_aerial(aerial2) .eq. 1
     :       .and. offset_aerial(aerial1) .eq. 0) then
          aerial = aerial2
       else
          write (iold, 1100)  aerial1,aerial2,text
 1100  format (1x, '*** unsuitable aerial pair : ',i1,'/',i1,
     :                 ',  offset aerials are ', A)
          goto 500
       endif

C  Define merge list using the specified merge-type
       call set_merge(sf_lun, sp_list, nspac, merge_type,
     :                       merge_list, no_groups, group_size, status)
       if (status.eq.0) then
          if (no_groups .ne. 1) then
             write (iold, *) '*** too many visibilities after merging'
             goto 500
          endif
       endif

       call enq_vis_desig(sf_lun, sp_list(1), isp, iba, ich, status)
       write (iout,'(1x,A,i1,A,1x,A)')
     :  ' offset data for aerial ', aerial, ', sub-band', sub_bands(iba)
       if (iold.ne. iout) then
         write (iold,'(1x,A,i1)') ' offset data for aerial ', aerial
       endif


C  Scan the sample file
       n = 0
       offsetx = -(offset2Nx+1)
       offsety = -(offset2Ny+1)
       do ibuff = ibuff1, ibuff2
          call lsf_set_buffer(lsf_num, ibuff, status)
          call lsf_get_vis(lsf_num, max_vis, vis_list, nspac, status)
          call merge_vis_buffer(vis_list, nspac,
     :                           merge_list, no_groups, group_size,
     :                           vis_merge, status)
          if (status.ne.0) then
             goto 999
          endif

C  Find offsets by dead reckoning

          if (mod(ibuff,nsize+1).eq.1) then
            haoff(ibuff) = 0.0
            decoff(ibuff) = 0.0
            offsety = offsety+1
            offsetx = -(offset2Nx+1)
          else
            offsetx = offsetx+1
            haoff(ibuff) = offsetx*offset_angle
            decoff(ibuff) = offsety*offset_angle
          endif

C  Find HA and amplitude of each merged visibility

          delay = 0.d0
          dummy = 0.d0
          elev = 0.d0
          call lsf_get_sid(lsf_num, sid, status)
          ha = 2.0*pi*sid/864000.0 - radate
          if (ha .gt. pi) ha = ha - 2*pi
          if (ha .lt.-pi) ha = ha + 2*pi
          hour_angle(ibuff) = ha
          amp(ibuff) = cabs(vis_merge(1))
          phi(ibuff) = 0.0
          if (vis_merge(1).ne.(0.0,0.0)) then
             phi(ibuff) = atan2(imag(vis_merge(1)),real(vis_merge(1)))
          endif

          if (Lprint) then
             write(iout,'(i4,i8,2F6.2,i6,F6.2,2E12.4,2f7.1)')
     :                                     ibuff,sid,
     :                                     elev,delay,integ,dummy,
     :                                     amp(ibuff),phi(ibuff),
     :                                     haoff(ibuff),decoff(ibuff)
          endif

       enddo
       write(iout,*)

C
       if (io_yesno('another spacing ?', 'no', status)) goto 500

C  Tidy up

  999  call io_close(iout, status)
       call io_setout(iold)
       write(iold,*)

       if (status.ne.0 .and. status.ne.usr_break) then
         call smp_wrerr(status, 'in routine ANAL_HOLOG_RYLE')
       endif

       end


