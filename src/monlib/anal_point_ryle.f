C+ANAL_POINT_RYLE

       subroutine anal_point_Ryle (lsf_num, plot_device, status)
C      ---------------------------------------------------------
C
C Analyse 5-point and raster offset observations
C
C  Given:
C      LSF_NUM       integer       logical sample file number
C      plot_device   char*(*)      PGplot device
C      STATUS        integer       status value
C
C  Scans the given logical sample file, for a pointing observation,
C  merging visibilities for each spacing (i.e. over sub-band/channel),
C  and displaying an analysis of the observed visibilities by pointing
C  offset.
C
C  The STATUS value should be zero on entry.
C
*-

* 7 July 93
* 8 Aug 97 : allow multi-5point obs; set subband merge (was channel)
* 13, 14, 18 Aug 97: new form of output file
* 28 11 97, 2 12 97: revised selection of spacings to be analysed
* 20 Aug 98: re-introduced fitting routine
*            commented-out references to the :5pt file
* 24 Aug 98   print summary results
* 25 Aug 98   plot_setmode ... text for summary results
* 15 Oct 98   PGbegin set for 4 plots/page
* 15 Oct 98   small change in spacing of printed values
* 28 Oct 98   transfer to RTSA
* 19 Sep 01   make it work for exactly 5 points

       implicit none

       integer        lsf_num, status
       character*(*)  plot_device


       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/merge_types.inc'
       include '/mrao/post/include/offset_types.inc'
       include '/mrao/include/iolib_functions.inc'
*      include '/mrao/include/sintran_errors.inc'
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/constants.inc'


       character     title(4)*80
       character     list*80, source*80, text*8
       character*64  file, heading(4), answer(2)
*      character*80   file_5pt
       character*1   offset_list(8)
       character*6   c_sp_list

       integer    ibuff, ibuff1, ibuff2, isamp, sf_lun, src_num, tscope
       integer    i, ii, iout, iae1, iae2, isp, iba, ich, j
       integer    merge_type
       integer    aerial, aerial1, aerial2
       integer    nspac, no_groups
       integer    offset_type, offset_time
       integer    offset_aerial(8), offset_ae_num, offset_plot_num
       integer    offset2Nx, offset2Ny, offset_zero, offset_row
       integer    ha_5point(0:4), dec_5point(0:4)
       integer    sm_type, sm_size, sm_rate, sid, i20
       integer    W_offset, E_offset, N_offset, S_offset, no_offset
       integer    n, nx, ny, wx, wy
       integer    refant

       real       ha, r20, ratio, sign, value, os, offset_angle
       real       W_amp, E_amp, N_amp, S_amp, on_amp
       real       WE_ratio, NS_ratio, WE_error, NS_error
       real       x, y, devx, devy
       real       sx, sy, sxx, syy, sh_x, shh_x, sh_y, shh_y, sxh, syh
       real       m_ha, c_ha,  m_dec, c_dec
       real       mean_off_ha, mean_off_dec
       real*8     epoch, radate, decdate, freq
       real*4     ha1, ha2
       real*4      mean_wind, min_wind, max_wind
       integer    sp_list(max_vis)
       integer    merge_list(2*max_vis), group_size(2*max_vis)

       complex    vis_list(max_vis), vis_merge(max_vis)

       real       hour_angle(max_samp), amplitude(max_samp)
       real       ha_point(max_samp),   dec_point(max_samp)
       real       mean_ha(max_samp)
       complex    visibility(max_samp)
       integer    ha_weight(max_samp), dec_weight(max_samp)


       integer    bytesperword
       parameter (bytesperword = 4)
       real       pi, min_amp
       parameter (pi = const_pi, min_amp = 0.05)


* table of offset angle af function of A/A', the ratio of amplitudes
* for offsets +- half voltage point of one aerial in a pair

* tabulated in arcseconds for A/A' = 1.00:0.05:3.00
* data from SK's table, last page of log book, volume 2
* [these values appropriate to 5 GHz: taken to scale as 1/f]


      integer point_error(0:40)/
     #      0,  9, 19, 28, 37, 45, 53, 61, 69, 76, 81,
     #         87, 93, 98,104,110,115,120,125,130,134,
     #        138,143,147,152,157,161,166,170,175,179,
     #        184,188,192,196,200,203,207,210,214,218/

      integer off_scale
      parameter (off_scale = 240)


*  Place work arrays in the work space common block
       common  /post/  sp_list, merge_list, group_size,
     :                 vis_list, vis_merge,
     :                 hour_angle, amplitude,
     :                 ha_point, dec_point, mean_ha


       if (status.ne.0) return

C   current output unit
       call io_enqout(iout)
       write (iout, *) '5point analysis: 19 Sept 2001'
C   sample file info
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

       merge_type = subband_merge
*      merge_type = channel_merge

* type of offset, aerials

      call enq_off_tables (sf_lun, offset_type, offset_aerial,
     #                     offset_angle, offset_time,
     #                     ha_5point, dec_5point,
     #                     offset2Nx, offset2Ny,
     #                     offset_zero, offset_row, status)

      if (offset_type .ne. o_5point .and. offset_type .ne. o_m5point)
     #                                                            then
          write (iout, *) 'not a 5-point offset'
          goto 9010
      endif

      text = '        '
      offset_ae_num = 0
      refant = 0
      do j = 1,8
          if (offset_aerial(j) .eq. 1) then
              text(j:j) = char(ichar('0')+j)
              offset_ae_num = offset_ae_num + 1
              offset_list(offset_ae_num) = text(j:j)
          else
              if (refant .eq. 0) refant = j   ! first non-offset aerial
          endif
      enddo

      if (offset_ae_num .eq. 0) then
          write (iout, *) 'no aerials offset ?'
          goto 9010
      else
          write (iout, *) ' aerials offset are: ', text
      endif
  50  call io_geti ('reference aerial :', '*', refant, status)
      if (status .ne. 0) goto 9010
      if (refant .lt. 1 .or. refant .gt. 8) goto 50

      write (iout, 1030) ' offset angle', offset_angle,' min arc; ',
     #                   offset_time, ' samples/point'
 1030 format(1x, A, f5.0, A, i3, A)

* find which elements correspond to which offsets

      W_offset = -1
      E_offset = -1
      N_offset = -1
      S_offset = -1
      no_offset= -1

      do i = 0, 4
          if ( ha_5point(i) .eq.  1)  W_offset = i
          if ( ha_5point(i) .eq. -1)  E_offset = i
          if (dec_5point(i) .eq.  1)  N_offset = i
          if (dec_5point(i) .eq. -1)  S_offset = i
          if ( ha_5point(i) .eq. 0 .and. dec_5point(i) .eq. 0)
     #                               no_offset = i
      enddo

      if    (W_offset .lt. 0 .or. E_offset .lt. 0
     # .or.  N_offset .lt. 0 .or. S_offset .lt. 0) then
          write (iout, *) 'incomplete offset table'
          goto 9010
      endif

      write (iout, '(1x, A, 5i2)')' offset samples (no, W, E, N, S) :',
     %       no_offset, W_offset, E_offset, N_offset, S_offset

*check sampling

      call LSF_enq_smooth (lsf_num, sm_type, sm_size, sm_rate, status)
      write (iout, 1002) sm_type, sm_size, sm_rate
 1002 format(1x, ' smooth type',i2,'  length',i3,'  sample-rate',i3)

      if (offset_time .ne. sm_size  .or.
     #    offset_time .ne. sm_rate) then
          write (iout, '(1x, a, i3)')
     *          'set smoothing and sampling to ',offset_time
          goto 9010
      endif


C  Get range of sample buffers
       call lsf_get_range(lsf_num, ibuff1, ibuff2, status)
       ibuff1 = ((ibuff1-1)/5)*5 + 1
       ibuff2 = (ibuff2/5)*5          ! whole no of cycles

       if (status.eq.0) then

              if  (ibuff2-ibuff1 .lt. 4)   then
                  write (iout, *) 'less than one offset cycle'
                  goto 9010
              endif
      endif


* wind-speed statistics
* ---------------------

       n=0
       mean_wind =    0.0
       max_wind  = -100.0
       min_wind  =  100.0
       do isamp = (ibuff1-1)*offset_time+1, (ibuff2-1)*offset_time+1
         call set_monitor(sf_lun,isamp,status)
         call enq_mon_wind(sf_lun,value,status )

         if (value.lt.min_wind) then
           min_wind = value
         elseif (value.gt.max_wind) then
           max_wind = value
         endif
         mean_wind = mean_wind + value
         n=n+1

       enddo

       mean_wind = mean_wind/n



         inquire (unit=sf_lun, name=file)
         write(iout,*) '5-point analysis: ', file
         write(iout,*)
       call lsf_title(lsf_num, list, ibuff1, ibuff2, title, status)

       write (iout,'(x,a)')title
       write (iout, 1050) mean_wind, min_wind, max_wind
1050   format (1x, 'Wind-speed statistics: ',
     *    '  mean :', f6.1, '  min :', f6.1, '  max:', f6.1,' knots')

       write (iout, *)

*       Lprint = YESNO('save the results ?', 'no', status)
*       if (Lprint) then
*          call brkfil(file, user, name, type)
*          i = index(user, ':')
*          user = user(i+1:)
*          type = '5pt'
*          call makfil (user, name, type, file_5pt, i)
*
*          call opefil (i_5pt, file_5pt, 'WRITE', 1, status)
*          write(i_5pt,*) '5-point analysis: ', file
*          write(i_5pt,*)
*          write (i_5pt, '(1x,''declination '', f8.3)')decdate/const_d2r
*          write (i_5pt,'(x,a)')title
*          write (i_5pt, 1050) mean_wind, min_wind, max_wind
*
*      endif

*      call pgbegin(0, plot_device, 1, 1)
       call pgbegin(0, plot_device, 2, 2)


      do offset_plot_num = 1, offset_ae_num

  500 write (c_sp_list,'(a,a,a,i1)')
     &      'ae ',offset_list(offset_plot_num),'/',refant
      if (io_YesNo('analyse '//c_sp_list//' ?', 'Yes', status)) then

*      write (*, *) c_sp_list, list, sp_list, max_vis, nspac, status
       call set_spacings(sf_lun,
     :       c_sp_list, sp_list, max_vis, nspac, status)
*      write (*, *) c_sp_list, list, sp_list, max_vis, nspac, status
       call lsf_set_spacings(lsf_num, nspac, sp_list, 2, status)
       call chr_chucas(list)
       if (status .ne. 0) goto 9000
       call lsf_enq_ae_vis(lsf_num, 1, aerial1, aerial2, status)
*      write (iout, '(1x, A, 2i2)') 'aerials', aerial1, aerial2
       do  i = 1, nspac
          call lsf_enq_ae_vis(lsf_num, i, iae1, iae2, status)
          if (status .ne. 0) goto 9000
          if (aerial1 .ne. iae1 .or. aerial2 .ne. iae2) then
              write (iout, *) 'More than one aerial pair included'
              goto 500
          endif
       enddo

* make sure that only one aerial is offset

       if      (offset_aerial(aerial1) .eq. 1
     # .and.   offset_aerial(aerial2) .eq. 0) then
          aerial = aerial1
       else if (offset_aerial(aerial2) .eq. 1
     # .and.   offset_aerial(aerial1) .eq. 0) then
          aerial = aerial2
       else
          write (iout, 1100)  aerial1,aerial2,text
 1100  format (1x, 'unsuitable spacing: ',i1,'/',i1,
     #            '  offset aerials are ', A)
          goto 500
       endif

C  Define merge list using the specified merge-type
       call set_merge(sf_lun, sp_list, nspac, merge_type,
     :                       merge_list, no_groups, group_size, status)

*      write (iout,       '(1x,A,i1)') '5-point analysis - ae ', aerial
       write (heading(1), '(1x,A,i1)') '5-point analysis - ae ', aerial
       write (heading(3), '(1x,A,2i5)')'LSF buffer range ',ibuff1,ibuff2

C  read the sample file
         n = 0
         do ibuff = ibuff1, ibuff2
           call lsf_set_buffer(lsf_num, ibuff, status)
           call lsf_get_vis(lsf_num, max_vis, vis_list, nspac, status)
           call merge_vis_buffer(vis_list, nspac,
     :                           merge_list, no_groups, group_size,
     :                           vis_merge, status)
           if (status.eq.0) then
              if (no_groups .ne. 1) then
                  write (iout, *) 'too many samples after merging'
                  write (iout, *) 'no_groups =', no_groups
                  goto 9000
              endif
           else
              goto 9000
           endif

* find HA and amplitude of each merged visibility

               call lsf_get_sid(lsf_num, sid, status)
               ha = 2.0*pi*sid/864000.0 - radate
               if (ha .gt. pi) ha = ha - 2*pi
               if (ha .lt.-pi) ha = ha + 2*pi
               hour_angle(ibuff) = ha
               amplitude (ibuff) = cabs(vis_merge(1))
               visibility(ibuff) = vis_merge(1)

         enddo

          ha1 = hour_angle(ibuff1)
          ha2 = hour_angle(ibuff2)


*      if (Lprint) then
*          write (i_5pt, *)
*          write (i_5pt,       '(1x,A,i1)') ' data for aerial ', aerial
*          write (i_5pt, *)
*     #     '  HA         W/E    P(h) wt     N/S    P(d) wt '
*      endif


          n = 0
          do i = ibuff1, ibuff2, 5
              n = n + 1
              W_amp = amplitude(i+W_offset)
              E_amp = amplitude(i+E_offset)
              N_amp = amplitude(i+N_offset)
              S_amp = amplitude(i+S_offset)
             on_amp = amplitude(i+no_offset)

                  if (W_amp .lt. min_amp .or. E_amp .lt. min_amp) then
                      WE_ratio = 1.0
                      ha_weight(n) = 0
                  else
                      WE_ratio = W_amp/E_amp
                      ha_weight(n) = 1
                  endif
                  if (N_amp .lt. min_amp .or. S_amp .lt. min_amp) then
                      NS_ratio = 1.0
                      dec_weight(n) = 0
                  else
                      NS_ratio = N_amp/S_amp
                      dec_weight(n) = 1
                  endif

* look up error in the table
*
*     if WE_ratio > 1.0 the aerial is pointing too far E
*     if NS_ratio > 1.0 the aerial is pointing too far S
*     both gave +ve output in SK's system

              if (WE_ratio .ge. 1.0) then
                  ratio = WE_ratio
                  sign = +1.0
              else
                  ratio = 1/WE_ratio
                  sign = -1.0
              endif

              if (ratio .gt. 3.0) then
                  WE_error = off_scale      ! off scale; nominal value
              else
                  r20 = (ratio - 1.0)*20.0
                  i20 = r20
                  WE_error = point_error(i20) +
     &              (point_error(i20+1)-point_error(i20))*(r20-i20)
              endif
              ha_point(n) = WE_error*sign*4995.0e6/freq


              if (NS_ratio .ge. 1.0) then
                  ratio = NS_ratio
                  sign = +1.0
              else
                  ratio = 1/NS_ratio
                  sign = -1.0
              endif

              if (ratio .gt. 3.0) then
                  NS_error = off_scale      ! off scale; nominal value
              else
                  r20 = (ratio - 1.0)*20.0
                  i20 = r20
                  NS_error = point_error(i20) +
     &              (point_error(i20+1)-point_error(i20))*(r20-i20)
              endif

              dec_point(n) = NS_error*sign*4995.0e6/freq
              mean_ha(n)   = hour_angle(i+3)/const_h2r    ! hours

*              if (Lprint) then
*
*                 write (i_5pt, 2010)
*     #                           mean_ha(n),
*     #                           WE_ratio, ha_point(n), ha_weight(n),
*     #                           NS_ratio,dec_point(n),dec_weight(n)
*
* 2010             format (1x,f6.3, 2(3x, f6.2, 2x, f6.0, 1x, i1))
*
*              endif

          enddo
*          if (Lprint) write (i_5pt, '(1x, ''/'')')


C
        if (status.ne.0) goto 9000

* analysis



* least-squares fits:  initialise sums
* ------------------------------------

* here x = ha pointing error, y = dec pointing error, h = hour angle
* sxh = sum of x*h etc
* sh_x, shh_x and sh_y, shh_y to allow for possible different weights

      sxx   = 0.0
      syy   = 0.0
      sxh   = 0.0
      syh   = 0.0

      sx    = 0.0
      sy    = 0.0
      sh_x  = 0.0
      shh_x = 0.0
      sh_y  = 0.0
      shh_y = 0.0
      nx    = 0
      ny    = 0

* least-sq fit : x[y] = m*h + c

* m = {n*sxh - sh*sx}/{n*shh - sh*sh}
* c = {sx*shh - sxh*sh}/{n*shh - sh*sh}




      do i = 1, n

          x  =  ha_point(i)
          y  = dec_point(i)
          ha = mean_ha(i)/12.0    ! need here HA as fr of pi
          wx =  ha_weight(i)
          wy = dec_weight(i)

          sh_x  = sh_x  + ha*wx
          shh_x = shh_x + ha*ha*wx*wx
          sh_y  = sh_y  + ha*wy
          shh_y = shh_y + ha*ha*wy*wy

          sx  = sx  + x*wx
          sy  = sy  + y*wy

          sxx = sxx + x*x*wx*wx
          syy = syy + y*y*wy*wy

          sxh = sxh + x*ha*wx
          syh = syh + y*ha*wy

          nx  = nx + wx
          ny  = ny + wy

      enddo

      if (abs(nx*shh_x-sh_x*sh_x) .lt. 1e-3) then
          write (iout, *) ' ill-conditioned ha data for fitting'
          m_ha = 0.0
          c_ha = 0.0
          devx = 0.0
*          o_aerial(aerial) = 0        ! no valid data in archive
      else

          m_ha  = (nx*sxh - sh_x*sx)/(nx*shh_x - sh_x*sh_x)
          c_ha  = (sx*shh_x - sxh*sh_x)/(n*shh_x - sh_x*sh_x)

          devx = 0.0
          do i = 1, n
              devx = devx
     %     + abs(ha_point(i)-m_ha*mean_ha(i)/pi-c_ha)*ha_weight(i)
          enddo
          devx = devx/nx
      endif

      mean_off_ha  = sx/nx

      if (abs(ny*shh_y-sh_y*sh_y) .lt. 1e-3) then
          write (iout, *) ' ill-conditioned dec data for fitting'
          m_dec = 0.0
          c_dec = 0.0
          devy  = 0.0
*          o_aerial(aerial) = 0        ! no valid data in archive
      else
          m_dec = (ny*syh - sh_y*sy)/(ny*shh_y - sh_y*sh_y)
          c_dec = (sy*shh_y - syh*sh_y)/(ny*shh_y - sh_y*sh_y)

          devy = 0.0
          do i = 1, n
              devy = devy
     %     + abs(dec_point(i)-m_dec*mean_ha(i)/pi-c_dec)*dec_weight(i)
          enddo
          devy = devy/ny

      endif

      mean_off_dec = sy/ny


         ii = 1
         do i = 1, no_groups
          if (i.eq.1) write(iout,*)
          call enq_vis_desig(sf_lun, sp_list(ii), isp, iba, ich, status)
          call enq_ae_vis(sf_lun, sp_list(ii), iae1, iae2, status)
          if (merge_type.eq.channel_merge) then
*             write (iout,       1091)   isp,iae1,iae2,char(iba+64)
              write (heading(2), 1091)   isp,iae1,iae2,char(iba+64)
 1091 format (1x, 'spacing :',i4,'  aerials: ',i1,',',i1,'  sb: ',A1)
     :
          elseif (merge_type.eq.subband_merge) then
*             write (iout,       1092)   isp,iae1,iae2
              write (heading(2), 1092)   isp,iae1,iae2
 1092 format (1x, 'spacing :',i4,'  aerials: ',i1,',',i1)
          endif

          ii = ii + group_size(i)

         enddo

          write (answer(1), '(x, 3(3x, a, i6))')
     *            'mean error ',nint(mean_off_ha),
     *            'slope ',nint(m_ha),  'intercept ',nint(c_ha)

          write (answer(2), '(x, 3(3x, a, i6))')
     *            'mean error ', nint(mean_off_dec),
     *            'slope ',nint(m_dec), 'intercept ',nint(c_dec)

       write (iout,'(x,A,i1, 3(x,A,i6), 2x, 3(x,A,i6))')
     *       ' ae ', aerial,
     *       'HA: mean ', nint(mean_off_ha),
     *       'm ', nint(m_ha),  'c ', nint(c_ha),
     *       'dec: mean ',nint(mean_off_dec),
     *       'm ', nint(m_dec), 'c ', nint(c_dec)

* plotting ....
* --------

Clear graphics device, draw plots, HA followed by Dec

* dummy values inserted to force standard scaling

               os = float((off_scale+10)*4995.0e6/freq)
               mean_ha (n+1) =  0.0
               mean_ha (n+2) = -6.0
               mean_ha (n+3) =  0.0
               mean_ha (n+4) = +6.0
               ha_point(n+1) =  0.0
               ha_point(n+2) =  os
               ha_point(n+3) =  0.0
               ha_point(n+4) = -os
              dec_point(n+1) =  0.0
              dec_point(n+2) =  os
              dec_point(n+3) =  0.0
              dec_point(n+4) = -os

             call pgask(.false.)
             call pgadvance

             call pgbbuf
             call plot_setmode('BRIEF', status)
             call pgvport( 0.1, 0.9, 0.45, 0.75 )
             call plot_data(n+4, mean_ha, ha_point, 1.0, 1.0, status)
             call pglabel(' ', 'W ... ha ... E', heading(1))
             call PGmtext ('B', -1.0, +0.02, 0.0, answer(1))
             call pgvport(0.1, 0.9, 0.1, 0.40)
             call plot_data(n+4, mean_ha, dec_point, 1.0, 1.0, status)
             call pglabel('Hour angle', 'N ... dec ... S', ' ')
             call PGmtext ('B', -1.0, +0.02, 0.0, answer(2))

c    title
             call pgvport( 0.1, 0.9, 0.85, 1.0 )
             call pgwindow( 0.0, 100.0, 4.2, -0.2 )
             call pgtext(0.0, 1.0, file)
             call pgtext(0.0, 2.0, heading(1))
             call pgtext(0.0, 3.0, heading(2))
             call pgtext(0.0, 4.0, heading(3))
             call plot_setmode('NORMAL',  status)
             call pgebuf

      endif

      enddo

      goto 9000


C  Tidy up

 9000  call pgend
 9010  write(iout,*)
*       if (Lprint)   close (i_5pt)

       if (status.eq.USR_BREAK) then
         status = 0
       elseif (status.ne.0) then
         call smp_wrerr(status, 'in routine ANAL_POINT_RYLE')
       endif

       end


