
*+PLOT_WTS_CLFST

       subroutine plot_wts_clfst (ifile, isamp1, isamp2, plot_device,
     :                                                          status)
C      ----------------------------------------------------------------
C
C  Plots sample weights for interference checks.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to produce plots of the sample weights (indicators of
C  interference rejection during the observing run) for the sample file
C  currently opened on logical unit IFILE, within the given sample range.
C  The plots are written to the current plot device.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 11/12/89]
*-
       character  plot_device*(*)
       integer    ifile, isamp1, isamp2
       integer    status
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  header(4)*80, text*24
       real*8     ra, dec, samp_ra, samp_dec
       integer    samp_status, samp_sid_time, samp_wt
       integer    ihead, isamp, istep, n
c
       integer      max_wt
       parameter   (max_wt=20)
c
       integer      no_samp
       parameter   (no_samp=800)
       real         xx(no_samp), yy(no_samp)
       common /post/ xx, yy
c
c
       if (status.ne.0) return
c
c  Initialise plot parameters.  The plot resolution is adjusted to
c  display a maximum of NO_SAMP samples spanning the given range.
c
       istep=(isamp2-isamp1)/no_samp+1
       call pgbegin( 0, plot_device, 1, 1 )
c
c  Initialise plot header text
c
       call enq_path_comp( ifile, ra, dec, status )
       text='Sample weights'
       call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
       header(3)(1:12)='Map centre'
c
c  Read sample weights from each sample
c
       n=0
       do isamp=isamp1,isamp2,istep
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,samp_ra,samp_dec,
     :                          samp_sid_time,samp_wt,status)
         if (status.eq.0) then
           n=n+1
           xx(n)=isamp
           yy(n)=samp_wt
         endif
       enddo
c
c  Produce plot
c
       if (status.eq.0) then
c
c    Clear graphics device, draw plot using fixed scaling
c
         call pmadvance( status )
         if (status .eq. 0) then
           call plot_setzeros( 'YES', status )
           call pgvport( 0.1, 0.9, 0.4, 0.8 )
           call plot_setscale( .false., 0.0, float(max_wt), status )
           call plot_data( n, xx, yy, 1.0, 1.0, status )
           call plot_setscale( .true., 0.0, 0.0, status )
           call pglabel( 'Sample number', 'Sample weight', ' ' )
c
c      Plot title
           call pgvport( 0.1, 0.9, 0.85, 1.0 )
           call pgwindow( 0.0, 100.0, 4.2, -0.2 )
           do ihead = 1, 4
             call pgtext( 0.0, real(ihead), header(ihead) )
           end do
         endif
       endif
c
c   Restore status if USR_BREAK is detected
       if ( status .eq. USR_BREAK ) status = 0
       call pgend
c
       end
