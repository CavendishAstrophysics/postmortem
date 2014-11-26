
*+PLOT_AGCS_CLFST

       subroutine plot_agcs_clfst (ifile, list, ilist, nae,
     :                              isamp1, isamp2, plot_device, status)
C      --------------------------------------------------------------
C
C  Plots AGC readings by aerial.
C
C  Given:
C      IFILE         integer       sample file logical unit number
C      LIST          char*(*)      aerial list
C      ILIST         integer(*)    aerial index numbers
C      NAE           integer       number of aerials in list
C      ISAMP1        integer       first sample
C      ISAMP2        integer       last sample
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to produce plots of the AGC values recorded with the sample
C  file currently opened on logical unit IFILE, for each aerial contained
C  in the given aerial list, within the given sample range.  The plots are
C  written to the current plot device.
C
C  The STATUS value should be zero on entry.
C
C [DJT, modified by PA for use with V1 and V2 CT, 9/11/88]
*-
       character  list*(*), plot_device*(*)
       integer    nae, ilist(nae)
       integer    ifile, isamp1, isamp2
       integer    status
c
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  header(4)*80, text*24
       real*8     ra, dec
       integer    i, iae, ipass, ihead, isamp, istep
       integer    mae, n
c
c  Maximum AGC value for plot axis
       integer      max_agc
       parameter   (max_agc=100)
c
       integer      no_ae, no_samp
       parameter   (no_ae=16, no_samp=800)
       real         xx(no_samp), yy(no_samp,no_ae)
       common /post/ xx, yy

       logical    test
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
       call enq_path_comp( ifile, ra, dec, status)
       text='Ae xx,  AGC values'
       call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
       header(3)(1:12)='Map centre'
c
c  Loop over aerials in the list, reading data for NO_AE aerials
c  during each pass.
c
       do ipass=1,nae,no_ae
         mae=min0(no_ae,nae-ipass+1)
c
         n=0
         do isamp=isamp1,isamp2,istep
           call set_monitor(ifile,isamp,status)
           if (status.eq.0) then
             n=n+1
             xx(n)=isamp
             do i=1,mae
               iae=ilist(ipass+i-1)
               call enq_mon_agc( ifile, iae, yy(n,i), status )
             enddo
           endif
         enddo
c
c    Produce plot for each aerial in turn, using fixed scaling
c
         call plot_setscale(.false., 0.0, float(max_agc), status )
c
         do i=1,mae
           iae=ilist(ipass+i-1)
           write(header(2)(4:5),'(i2)')iae
           call enq_aestat( ifile, iae, 1, test, status )
           if ( test .and. status.eq.0 ) then
c
c    Clear graphics device, draw plot using fixed scaling
c
             call pmadvance( status )
             if (status .eq. 0) then
c             Plot graph
               call plot_setzeros( 'YES', status )
               call pgvport( 0.1, 0.9, 0.4, 0.8 )
               call plot_data( n, xx, yy(1,i), 1.0, 1.0, status )
               call pglabel( 'Sample number', 'AGC level', ' ' )
c
c             Plot title
               call pgvport( 0.1, 0.9, 0.85, 1.0 )
               call pgwindow( 0.0, 100.0, 4.2, -0.2 )
               do ihead = 1, 4
                 call pgtext( 0.0, real(ihead), header(ihead) )
               end do
             endif
           endif
         enddo
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
