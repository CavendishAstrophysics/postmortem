
*+PLOT_POINT_RYLE

       subroutine plot_point_ryle (ifile, list, ilist, nae,
     :                    isamp1, isamp2, errtype, plot_device, status)
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
C      ERRTYPE       integer       error type
C      PLOT_DEVICE   char*(*)      PGPLOT device code
C
C  Returned:
C      STATUS        integer       status value
C
C  Subroutine to produce plots of the HA and Dec pointing errors recorded
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
       integer    errtype
       integer    status
c
       include '/mrao/post/include/5km_constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_errors.inc'
c
       character  header(4)*80, text*32
       real*8     ra, dec
       integer    i, iae, ihead, isamp, istep
       integer    n
c
       real       xx(max_samp)
       real       ha_err(max_samp,max_aes)
       real       dec_err(max_samp,max_aes)
       common /post/ xx, ha_err, dec_err

       logical    test
       integer    iha(2), idec(2)
c
       if (status.ne.0) return
c
c  Initialise plot parameters.  The plot resolution is adjusted to
c  display a maximum of max_samp samples spanning the given range.
c
       istep=(isamp2-isamp1)/max_samp+1
       call pgbegin( 0, plot_device, 1, 1 )
c
c  Initialise plot header text
c
       call enq_path_comp(ifile,ra,dec,status)
       text='Ae xx,  Aerial pointing errors'
       call mon_title(ifile,text,isamp1,isamp2,istep,ra,dec,header)
       header(3)(1:12)='Map header'
c
c  Loop over aerials in the list, reading data for max_aes aerials
c  during each pass.
c
       n=0
       do isamp=isamp1,isamp2,istep
         call set_monitor(ifile,isamp,status)
         if (status.eq.0) then
           n=n+1
           xx(n)=isamp
           do i=1,nae
             iae=ilist(i)
             call enq_mon_hadec(ifile,iae,iha,idec,status)
             ha_err(n,i)=iha(errtype)
             dec_err(n,i)=idec(errtype)
           enddo
         endif
       enddo
c
c    Produce plots for each aerial in turn
c
       do i=1,nae
         iae=ilist(i)
         write(header(2)(4:5),'(I2)')iae
         call enq_aestat( ifile, iae, 1, test, status)
         if (test .and. status.eq.0 ) then
c
c    Clear graphics device, draw plots, HA followed by Dec
c
           call pmadvance( status )
           if ( status .eq. 0 ) then
c           Plot graphs
             call pgbbuf
             call plot_setzeros( 'YES', status )
             call pgvport( 0.1, 0.9, 0.45, 0.75 )
             call plot_data( n, xx, ha_err(1,i), 1.0, 1.0, status )
             if (errtype.eq.1) then
               call pglabel( ' ', 'HA error', 'Mean errors' )
             elseif (errtype.eq.2) then
               call pglabel( ' ', 'HA error', 'Mean absolute errors' )
             endif
             call pgvport( 0.1, 0.9, 0.1, 0.40 )
             call plot_data( n, xx, dec_err(1,i), 1.0, 1.0, status )
             call pglabel( 'Sample number', 'Dec. error', ' ' )
c
c           Plot title
             call pgvport( 0.1, 0.9, 0.85, 1.0 )
             call pgwindow( 0.0, 100.0, 4.2, -0.2 )
             do ihead = 1, 4
               call pgtext( 0.0, real(ihead), header(ihead) )
             end do
             call pgebuf
           endif
         endif
       enddo
c
c     Restore status if USR_BREAK is detected
       if ( status .eq. USR_BREAK ) status = 0
       call pgend
c
       end
