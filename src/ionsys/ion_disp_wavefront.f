C
C+ion_disp_wavefront
C
      SUBROUTINE ion_disp_wavefront(  sf_name,
     *                            plot_device,
     *                            s              )

C
C     Plots aerial or hut merges of samples vs x on the plot device
C         fits second order curve to resulting phases
C
C     Given:
C         Logical sample file name
              integer*4           sf_name
C         PGPLOT plot device
              character*(*)       plot_device
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Runs through the current logical sample file displaying (optional)
C     aerial-merges against x-geom buffer by buffer.
C     Slope and curvature saved for optional plot against time.
C                                                         PJW April '89
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/global_constants.inc'
      include  '/mrao/post/include/merge_types.inc'
      include  '/mrao/post/include/lsfun1.inc'
      include  '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         General purpose loop counters
              integer         i, iae
C         LSF open no.
              integer         lsf_num, sf_lun
C         Plot title
              character*(80)  title(4)
C         string description.
              character*(80)  list
C         x geometry of each aerial, and constants
              real            x_geom( max_aes ), geomf(3), geoml(3)
              real*8          freq, wavel_km
C         List of visibilities returned from get_vis_buffer
              complex         vis_list( max_vis )
C         aerial and hut merging parameters  and arrays
              integer         merge_type
              integer         mspac( max_aes ), mae, iaef, iael
              integer         sp_list( max_vis )
              integer         naehut
              integer         ilist( 2*max_vis )
              complex         vis_ae( max_aes ), vis_buf( max_aes )
              integer         neast, nwest, max_east
              real            mean_west, sum_amp
C         Number of spacings
              integer         num_spac
C         Current and last LSF buffer to display
              integer         buff_num, last_buff
C         Sampling rate in LSF buffers.
              integer         samp_rate
C         Plot type for plot_complex
              integer         plot_type
C         variables for naglib call E04FDF
              real*8          w(600)
              real*8          fsumsq, abc(3)
              integer         m, n, iw(1), liw, lw, ifail
C         slope and curvature arrays and number of points
              real            slope( max_samp ), curvature( max_samp )
              real            time( max_samp )
              integer         npoint, sid
              logical         io_yesno, plot_samp, plot_slopes
C         Postmortem common
              common / post / w, x_geom, vis_list, ilist

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      plot_type = 0

      call lsf_open ( sf_name, ion_lsf, 'READ', lsf_num, s )
      call lsf_set_pc( lsf_num, ion_epoch, ion_ra(1), ion_dec(1),
     *                 ion_source(1), s )
      call lsf_enq_sf( lsf_num, sf_lun, i, s )

      call enq_freq( sf_lun, freq, s)
      wavel_km = const_c/( freq*1000.)

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call get_spacings( sf_lun, 'Spacing list : ', 'hut a-d/e-g',
     *                   list, sp_list, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac, sp_list, 2, s )

      call get_merge( sf_lun, 'merge type:', 'hut-merge',
     *                       merge_type, s )
      call set_merge( sf_lun, sp_list, num_spac, merge_type,
     *                ilist, mae, mspac, s )
C      write(1,*)merge_type,mae,mspac
      mean_west = 0.
      neast = 0
      nwest = 0
      naehut = 0
      if( merge_type .eq. aerial_merge ) then
          do 50 i = 1,mae
              if( mspac(i) .gt. 0 ) then
                  naehut = naehut + 1
                  call enq_chgeom( sf_lun, i, 1, 1, geomf, s )
                  x_geom(naehut) = geomf(1) * wavel_km
                  pos(naehut) = x_geom(naehut)
                  if( i .le. 28 ) then
                      neast = neast + 1
                  else
                      mean_west = mean_west + x_geom(naehut)
                      nwest = nwest + 1
                  endif
              endif
50        continue
      else
          do 55 i = 1,mae
              if( mspac(i) .gt. 0 ) then
                  naehut = naehut + 1
                  call enq_ae_hut( sf_lun, i, iaef, iael, s )
                  call enq_chgeom( sf_lun, iaef, 1,1, geomf, s)
                  call enq_chgeom( sf_lun, iael, 1,1, geoml, s)
                  x_geom(naehut) = ( geomf(1) + geoml(1) )*wavel_km/2.
                  pos(naehut) = x_geom(naehut)
                  if( i .le. 4 ) then
                      neast = neast + 1
                  else
                      mean_west = mean_west + x_geom(naehut)
                      nwest = nwest + 1
                  endif
              endif
55        continue
      endif
      mean_west = mean_west/nwest

      call lsf_get_range( lsf_num, buff_num, last_buff, s )
      call io_geti( 'Sampling rate : ', '1', samp_rate, s )

      if( io_yesno(
     *        'Plot samples against x-geometry ?', 'yes', s ) ) then
          plot_samp = .true.
      else
          plot_samp = .false.
      endif
      if( io_yesno(
     *   'plot slopes + curvatures against time?', 'yes', s ) ) then
          plot_slopes = .true.
      else
          plot_slopes = .false.
      endif

      npoint = 0
      call pgbegin( 0, plot_device, 1, 1 )

  100 if ((s .ne. 0) .or. (buff_num .gt. last_buff)) goto 1000

      call lsf_set_buffer(lsf_num, buff_num, s )
      call lsf_get_sid( lsf_num, sid, s )
      call lsf_get_vis(  lsf_num, max_vis,vis_list,num_spac, s)
      call merge_vis_buffer( vis_list, num_spac, ilist, mae, mspac,
     *                                  vis_ae, s )
      call lsf_title( lsf_num, list, buff_num, -1, title, s )

      naehut = 0
      sum_amp = 0.
      if ( merge_type .eq. hut_merge ) then
          max_east = 4
      else
          max_east = 28
      endif
      do 110 iae = 1, max_east
          if( mspac(iae) .gt. 0 ) then
              naehut = naehut + 1
              vis_buf(naehut) = vis_ae( iae )
              sum_amp = sum_amp + cabs( vis_buf(naehut) )
              phase(naehut) = atan2(imag(vis_buf(naehut)),
     *                              real(vis_buf(naehut))) / const_d2r
          endif
110   continue
      vis_buf(naehut+1) = cmplx(sum_amp/naehut,0.01)
      phase(naehut+1) = 0.
      pos(naehut+1) = mean_west
      x_geom(naehut+1) = mean_west
      m = naehut + 1
      n = 3
      liw = 1
      abc(1) = 0.
      abc(2) = 0.
      abc(3) = 0.
      lw = 600
      ifail = 1
C      write(1,*)phase,pos
      call set_E04FDF_mode( 'ION-DISPLAY-WAVE', s )
      call E04FDF( m, n, abc, fsumsq, iw, liw, w, lw, ifail)
      write(1,888) abc,fsumsq,ifail
888   format(' fit: ',3F8.1,'   residual: ',G12.2,'   NAG ifail: ',I3)
C      write(1,*)m,vis_buf,x_geom

      npoint = npoint + 1
      slope(npoint) = abc(2)
      curvature(npoint) = abc(3)
      time(npoint) = float(sid) / 36000.

      if ( plot_samp ) then
          call plot_setmode( 'SCALED-PHI', s )
          call plot_complex( title,
     *                   'Merge A and phi vs x geometry',
     *                   'x geometry(km)',
     *                    vis_buf,
     *                    m, 1,
     *                    x_geom,
     *                    plot_device,
     *                    plot_type,
     *                    s               )
          buff_num = buff_num+samp_rate
          call plot_setmode( 'NORMAL', s )
      endif

      goto 100
 1000 continue

      if ( plot_slopes ) then
            call pgbbuf
            call pmadvance( s )
            call pgvport( 0.1, 0.9, 0.55, 0.9 )
            call plot_data( npoint, time, slope, 1.0, 1.0, s )
            call pglabel( 'Sidereal time (decimal hrs)',
     *                     'Slope (deg/km)', title )
            call pgvport( 0.1, 0.9, 0.1, 0.45 )
            call plot_data( npoint, time, curvature, 1.0, 1.0, s )
            call pglabel('','Curvature (deg/km.km)',' ')
            call pgebuf
      endif

      call pgend
      if ( s .ne. 0 ) goto 9999
      call lsf_close( lsf_num, s )

      if ( s .eq. 0 ) return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .eq. usr_break ) then
              s = 0
              call lsf_close( lsf_num, s )
          else
              call ion_wrerr( s, ' in subroutine ion_disp_wavefront ' )
          end if
          return
      end
