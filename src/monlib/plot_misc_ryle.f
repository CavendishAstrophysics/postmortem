*+PLOT_MISC_RYLE

       subroutine plot_misc_ryle (ifile, ilist, npar,
     :                           isamp1, isamp2, plot_device, status)
C      --------------------------------------------------------------
C
C  Plots miscellaneous parameter readings.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      ILIST         integer(*)    parameter index numbers
C      NPAR          integer       number of parameters in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only]
C
C  Subroutine to produce plots of parameter readings recorded with the
C  sample file currently opened on logical unit IFILE, within the given
C  sample range.  The plots are written to the current plot device.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 22/2/93]
*-
       character  plot_device*(*)
       integer    npar, ilist(npar)
       integer    ifile, isamp1, isamp2
       integer    status
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  header(4)*80, text*24
       real*4     vp(4,2)
       real*8     ra, dec
       integer    samp_status, sid_time, wt
       integer    ipar, ihead, isamp, istep
       integer    n
c
c  Maximum value for plot axis
       real         max_value
       parameter   (max_value=10.0)
c
       integer      no_samp
       parameter   (no_samp=800)
       integer      max_pars
       parameter   (max_pars=3*max_aes)
       real         xx(no_samp), yy(no_samp,max_pars)
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
       text='Parameter xx values'
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
           do ipar=1,npar
             call enq_mon_misc( ifile, ilist(ipar), yy(n,ipar), status )
           enddo
         endif
       enddo
c
c    Produce plot for each aerial in turn, using fixed scaling
c
       call plot_setvp(1, 1, vp, status )
       call plot_setscale(.false., 0.0, max_value, status )
c
       do ipar=1,npar
         write(header(2)(11:12),'(i2)')ilist(ipar)
c
c    Clear graphics device, draw plot
c
         call pmadvance( status )
         if (status .eq. 0) then
c         Plot graph
           call pgbbuf
           call plot_setzeros( 'YES', status )
           call pgvport( vp(1,1), vp(2,1), 0.4, 0.8 )
           call plot_data( n, xx, yy(1,ipar), 1.0, 1.0, status )
           call pglabel( 'ST (hrs)', '(volts)', ' ' )
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


