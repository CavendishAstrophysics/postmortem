
*+PLOT_POINT_CLFST

       subroutine plot_point_clfst (ifile, list, ilist, nae,
     :                             isamp1, isamp2, plot_device, status)
C      ----------------------------------------------------------------
C
C  Plots aerial pointing data.
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
C  Subroutine to produce plots of the HA and Dec pointing data recorded
C  within the sample file currently opened on logical unit IFILE, for
C  each aerial included in the given aerial list, within the specified
C  sample range.  The plots are written to the current plot device.
C
C  The STATUS value should be zero on entry.
C
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
       character  header(4)*80, text*32
       real*8     ra, dec
       integer    i, iae, ipass, ihead, isamp, istep
       integer    mae, n
c
       integer    no_ae, no_samp
       parameter (no_ae=8)
       parameter (no_samp=800)
c
       real       xx(no_samp)
       real       ha_err(no_samp,no_ae)
       real       dec_err(no_samp,no_ae)
       common /post/ ha_err, dec_err

       logical    test
       integer    iha(2), idec(2)
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
       call enq_path_comp(ifile,ra,dec,status)
       text='Ae xx,  Aerial pointing errors'
       call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
       header(3)(1:12)='Map header'
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
               call enq_mon_hadec(ifile,iae,iha,idec,status)
               ha_err(n,i)=iha(2)-iha(1)
               dec_err(n,i)=idec(2)-idec(1)
             enddo
           endif
         enddo
c
c    Produce plots for each aerial in turn
c
         do i=1,mae
           iae=ilist(ipass+i-1)
           write(header(2)(4:5),'(I2)')iae
           call enq_aestat( ifile, iae, 1, test, status)
           if (test .and. status.eq.0 ) then
c
c    Clear graphics device, draw plots, HA followed by Dec
c
             call pmadvance( status )
             if ( status .eq. 0 ) then
c             Plot graphs
               call plot_setzeros( 'YES', status )
               call pgvport( 0.1, 0.9, 0.45, 0.75 )
               call plot_data( n, xx, ha_err(1,i), 1.0, 1.0, status )
               call pglabel( ' ', 'HA error', 'Aerial pointing errors' )
               call pgvport( 0.1, 0.9, 0.1, 0.40 )
               call plot_data( n, xx, dec_err(1,i), 1.0, 1.0, status )
               call pglabel( 'Sample number', 'Dec. error', ' ' )
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
       call pgend
c
       end
