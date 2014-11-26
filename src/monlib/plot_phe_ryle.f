*+PLOT_PHE_RYLE

       subroutine plot_phe_ryle (ifile, list, ilist, nae,
     :                 isamp1, isamp2, max_value, plot_device, status)
C      ---------------------------------------------------------------
C
C  Plots Helium pressures by aerial.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      MAX_VALUE     real          maximum value
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  [RYLE Telescope only]
C
C  Subroutine to produce plots of the Helium pressure readings recorded
C  with the sample file currently opened on logical unit IFILE, for each
C  aerial contained in the given aerial list, within the given sample
C  range.  The plots are written to the current plot device.
C
C  The STATUS value should be zero on entry.
C
C  [DJT, 23/9/94]
*-
       character  list*(*), plot_device*(*)
       real       max_value
       integer    nae, ilist(nae)
       integer    ifile, isamp1, isamp2
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
       text='Ae xx,  Helium pressure readings'
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
             call enq_mon_phe( ifile, iae, yy(n,i), status )
           enddo
         endif
       enddo
c
c    Produce plot for each aerial in turn, using fixed scaling
c
       call plot_setvp(1, 1, vp, status)
       call plot_setscale(.false., 0.0, max_value, status )
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
           call pglabel( 'ST (hrs)', 'Helium pressure', ' ' )
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


