C
C
C+lsf_fit_adc

      subroutine lsf_fit_adc( sf_lun, vis, time, lw, work,
     *                        num_buff, interval, s)
C
C Fit the supplied spacing data with a function Acos(ha)+Dsin(ha)+C
C
C Given:
C    sample file logical unit number
       integer        sf_lun
C    visibilities
       complex        vis(*)
C    time (sid hrs)
       real           time(*)
C    length of work array
       integer        lw
C    work array
       real*8         work(lw)
C    number of sample buffers
       integer        num_buff
C    interval to skip in vis array
       integer        interval
C Returned:
C    status word
       integer        s
C
C The data are fitted with a phase function of the form:
C
C     Acos(ha) + Dsin(ha) + C
C
C The results, the parameters A, D and C can be found by using the
C equiry routine lsf_enq_adc.  The minization is performed using the
C NAG routine E04FDF with MODE='FIT-SPACING'.
C
C PA, 10/10/90
C-
       include '/mrao/include/constants.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/lsfun1.inc'

C local variables
       integer      m, n, i, j, ifail
       real*8       xc(3), ratel, fsumsq, ph_tot, ph_old
       integer      liw
       parameter   (liw = 100)
       integer      iwork(liw)


C save results of fit
       real*4                 save_ratel, save_ADC(3)
       common /save_fit_res/  save_ratel, save_ADC

       if (s.ne.0) return

C set mode for minimization
       call set_E04FDF_mode( 'FIT-SPACINGS', s )

C enquire RATEL
       call enq_ratel( sf_lun, ratel, s )
       save_ratel = ratel

C sort out data to fit
       n = 3
       do i=1,3
         xc(i) = 0.0D+0
       end do
       j = 1
       m = 0
       ph_tot = 0.0D+0
       ph_old = 0.0D+0
       do i=1,num_buff
         if (vis(j).ne.(0.0,0.0)) then
           m = m+1
           vis_phase(m) = atan2(imag(vis(j)),real(vis(j)))/const_d2r
           vis_ha(m) = time(i)*const_2pi/24.0 - ratel
         end if
         j = j + interval
         vis_phase(m) = vis_phase(m) + ph_tot
         if (abs(vis_phase(m)-ph_old).gt.180.0) then
           if (vis_phase(m).lt.ph_old) then
             ph_tot = ph_tot + 360.0
             vis_phase(m) = vis_phase(m) + 360.0
           else
             ph_tot = ph_tot - 360.0
             vis_phase(m) = vis_phase(m) - 360.0
           end if
         end if
         ph_old = vis_phase(m)
       end do
       if (s.ne.0) goto 999

C do the fit
       ifail = 1
       call e04fdf(m,n,xc,fsumsq,iwork,liw,work,lw,ifail)
       if (ifail.ne.0) then
         if (ifail.eq.1) then
          call io_wrout('***(FIT-SPACING) Error calling E04FDF Ifail=1')
         else if (ifail.eq.2) then
          call io_wrout('***(FIT-SPACING) Error calling E04FDF Ifail=2')
         else if (ifail.eq.3) then
          call io_wrout('***(FIT-SPACING) Error calling E04FDF Ifail=3')
         else if (ifail.eq.4) then
          call io_wrout('***(FIT-SPACING) Error calling E04FDF Ifail=4')
         else if (ifail.ge.5) then
           continue
*         call io_wrout('***(FIT-SPACING) Warning ifail>=5 in E04FDF')
         end if
       end if


C save results
       do i=1,3
         save_ADC(i) = xc(i)
       end do

999    continue
       if (s.ne.0) call io_wrerr(s,'in lsf_fit_ADC')
       end
