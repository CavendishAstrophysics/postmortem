
*+PLOT_RAIN_GAUGE

       subroutine plot_rain_gauge (ifile, list, ilist, nae,
     :                     isamp1, isamp2, ivar, plot_device, status)
C      --------------------------------------------------------------
C
C  Plots rain gauge readings by aerial.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      IVAR          integer       selects display variable
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only]
C
C  Subroutine to produce plots of the rain gauge readings recorded with
C  the sample file currently opened on logical unit IFILE, for each aerial
C  contained in the given aerial list, within the given sample range.
C  The plots are written to the current plot device.
C
C  If IVAR=1, the rain gauge readings are displayed.
C  If IVAR=2, the derived rain correction factors are displayed.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 3/4/90]
*-
       character  list*(*), plot_device*(*)
       integer    nae, ilist(nae)
       integer    ifile, isamp1, isamp2, ivar
       integer    status
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  header(4)*80, text*32
       real*4     vp(4,2)
       real*8     ra, dec
       integer    samp_status, sid_time, wt
       integer    i, iae, ihead, isamp, istep
       integer    n
c
c  Maximum value for plot axis
       real         max_rain
c
       integer      no_samp
       parameter   (no_samp=800)
       real         xx(no_samp), yy(no_samp,max_aes)
       common /post/ xx, yy

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
       max_rain = 10.0
       if (ivar.eq.1) then
         text='Ae xx,  rain gauge values'
       elseif (ivar.eq.2) then
         text='Ae xx,  rain correction factors'
       endif
       call enq_path_comp(ifile,ra,dec,status)
       call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
       header(3)(1:12)='Map centre'
c
c  Loop over aerials in the list
c
       n=0
       do isamp=isamp1,isamp2,istep
         call set_monitor(ifile,isamp,status)
         call enq_samp_rt(ifile,samp_status,ra,dec,sid_time,wt,status)
         if (status.eq.0) then
           n=n+1
           xx(n)=sid_time/36000.0
           do i=1,nae
             iae=ilist(i)
             if (ivar.eq.1) then
               call enq_mon_rain( ifile, iae, yy(n,i), status )
             elseif (ivar.eq.2) then
               call enq_mon_rfac( ifile, iae, yy(n,i), status )
             endif
           enddo
         endif
       enddo
c
c    Produce plot for each aerial in turn, using auto scaling for the
c    dervied correction factors.
c
       call plot_setvp(1, 1, vp, status )
       if (ivar.eq.1) then
         call plot_setscale(.false., 0.0, max_rain, status )
       elseif (ivar.eq.2) then
         call plot_setscale(.true., 0.0, 0.0, status )
       endif
c
       do i=1,nae
         iae=ilist(i)
         write(header(2)(4:5),'(i2)')iae
c
c    Clear graphics device, draw plot
c
         call pmadvance( status )
         if (status .eq. 0) then
c         Plot graph
           call pgbbuf
           call plot_setzeros( 'YES', status )
           call pgvport( vp(1,1), vp(2,1), 0.4, 0.8 )
           call plot_data( n, xx, yy(1,i), 1.0, 1.0, status )
           if (ivar.eq.1) then
             call pglabel( 'ST (hrs)', 'rain gauge (volts)', ' ' )
           elseif (ivar.eq.2) then
             call pglabel( 'ST (hrs)', 'rain correction factors', ' ' )
           endif
c
c         Plot title
           call pgvport( 0.1, 0.9, 0.85, 1.0 )
           call pgwindow( 0.0, 100.0, 4.2, -0.2 )
           do ihead = 1, 4
             call pgtext( 0.0, real(ihead), header(ihead) )
           end do
           call pgebuf
         endif
       enddo
c
c     Restore status if USR_BREAK is detected
       if ( status .eq. USR_BREAK ) status = 0
c
c     Restore automatic scaling as default
       call plot_setscale( .true., 0.0, 0.0, status )
       call pgend
c
       end
