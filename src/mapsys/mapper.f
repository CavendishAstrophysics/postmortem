C
C+mapper
C
      subroutine mapper (map_name, s)

C
C     Makes a map using the redtape in the specified file.
C
C     Given:
C         Full io_system map file name.
              character*(*)       map_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Opens the file given by map_name, which must be a valid map file,
C     and makes a map from the definition in the map red tape.
C
C     NPR     throughout 1987.
C     PJW modified to include beamsets with integration-time smearing 8/91
C     GP  9 May 2000: added MEJ code (1992!) for rain-gauge weighting
C     GP  6 Feb 2003: hid some messages 
C-
C     ****************************************************************
C
C     Function declarations -
C
      include '/mrao/include/chrlib_functions.inc'
C
C     ****************************************************************
C
C     Global includes - (constant, variable and common declarations)
C
      include '/mrao/include/constants.inc'
      include '/mrao/include/maplib_redtape.inc'
      include '/mrao/include/maplib_errors.inc'
      include '/mrao/post/include/global_constants.inc'
      include '/mrao/post/include/weighting_types.inc'

              intrinsic btest
              logical   btest

C     FFTLIB logical function to check an integer for power of 2
              external  check2
              logical   check2

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
C         Maximum size of aperture and map array.
              integer         max_buff_size
              parameter     ( max_buff_size = 4*(513*1024) )
C         Convolution function half width.
              integer         max_conv_hw
              parameter     ( max_conv_hw = 400 )

C     Variables, equivalences and commons
C         Loop counters
              integer         i, j, k
C         Real, complex and integer declarations of the workspace.
              real            rbuff( max_buff_size   )
              complex         cbuff( max_buff_size/2 )
              integer         ibuff( max_buff_size   )
              equivalence   ( rbuff, cbuff )
              equivalence   ( rbuff, ibuff )
C         Pointers within these buffers to the start of the map,
C         aperture, beam aperture and super-smooth weighting arrays.
              integer         map_ptr, aper_ptr, beam_ptr, smooth_ptr
C         Pointer in the buffer to the start of a work array
              integer         work_ptr
C         Number of elements and columns in the aperture half plane.
              integer         aper_size, aper_cols
C         Tabulated convolution function.
              real            conv( -max_conv_hw:max_conv_hw )
C         Current sample file name and lsf key.
              character*80    file_name
              integer         lsf_key
C         Current lsf number and number of buffers in lsf.
              integer         lsf_num, num_buff
C         Current number of visibilities in buffer
              integer         num_vis
C         Number of valid visibilities in buffer
              integer         val_vis
C         Visibility buffer
              complex         vis( max_vis )
C         UV coordinate buffer for visibilities
              real            uv( 2, max_vis )
C         Weights for each visibility.
              real            weight( max_vis )
C         Table of aerial pairs for each visibility
              integer         samp_aes( max_vis, 2 )
C         Pair of aerials
              integer         ae1, ae2
C         List of aerial system temperatures
              real            Tsys( max_aes )
C         Nominal rain gauge value
              real            nominal_rg
C         Overall weight of aperture due to rain gauge weighting
              real            rg_weight

C         UV plane skew angle
              real*8          skew_angle
C         Map statistics from maplib routine SCNMAP
              real            stats(4)
              integer         posns(4)
C         Number of gridpoints/wavelength in u and v
              real            uwl2gp, vwl2gp
C         Map and sample-file fortran logical unit numbers
              integer         lun, slun
C         source no in lsf
              integer         lsf_source
C         Beam file name, and map name user and type
              character       beam_name*64, user*33, type*4
C         String and length
              character*47    string
              integer         ls
C         Model definition variables
              real*8          u, v, ra, dec
              complex         model_vis( max_vis )
C         Runtime statistics and times.
              integer         tot_val, tot_vis
              real*4          cpu, elapsed
              real*4          fft_cpu, fft_elapsed
              real*4          int_cpu, int_elapsed
              real*4          smoo_cpu, smoo_elapsed
C         Execution mode information
              character*16    current_user
              integer         exe_mode, termno

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Find execution mode
      call io_enqexe( current_user, exe_mode, termno )
C     Initialise redtape to minimalist values to avoid OPEMAP problems
      call nwredt( 2, 2, 3, 1, s )

C     Open map and read map redtape.
      call opemap( lun, map_name, 'WRITE', 0, s )
      call rdredt( lun, 0, s )
      if ( s .ne. 0 ) goto 9999

C     Check to see that the map dimensions are powers of two.
      if ( .not.check2(ixmax) .or. .not.check2(iymax)) then
          s = ILL_REDTAPE
          goto 9999
      end if

C     Check to see there is enough space to make this map.
      aper_cols = ixmax/2+1
      aper_size = aper_cols*iymax
      if ( mod(wghttp,10) .eq. super_smooth .or.
     *     mod(wghttp,10) .eq. noise_wt          ) then
          if ( (aper_size*4) .gt. max_buff_size ) s = ARR_TOOSMALL
      else
          if ( (aper_size*3) .gt. max_buff_size ) s = ARR_TOOSMALL
      end if

C     Set up buffer pointers.
      map_ptr    = 1
      aper_ptr   = 1
      work_ptr   = 1
      beam_ptr   = map_ptr   + aper_size*2
      smooth_ptr = beam_ptr  + aper_size

C     Initialise timing variables
      smoo_cpu    = 0.0
      smoo_elapsed= 0.0
      int_cpu     = 0.0
      int_elapsed = 0.0
      fft_cpu     = 0.0
      fft_elapsed = 0.0

C     Set up projection parameters.
      if ( iproj .eq. 1 ) then
C Equatorial projection
          uwl2gp = real(ixmax) * usamp * const_sa2r
          vwl2gp = real(iymax) * usamp * const_sa2r
      else if (   iproj.eq.2          .and.
     *            skew.eq.0.0D+0      .and.
     *            epoch.eq.obsdat     .and.
     *            -iumap1.eq.iumap2+1 .and.
     *            ivmap1.eq.-ivmap2+1         ) then
C Standard, central, sky projection
          uwl2gp = real(ixmax) * usamp * const_sa2r
          vwl2gp = real(iymax) * usamp * const_sa2r * dsin( decobs )
      else
          s = ILL_REDTAPE
          goto 9999
      end if


C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( mod(wghttp,10) .eq. super_smooth .or.
     *     mod(wghttp,10) .eq. noise_wt          ) then
        call io_wrout( 'Calculating super-smooth weighting array ...' )
        call util_stclck
        call get_smooth_array(  rbuff(smooth_ptr),
     *                          aper_cols, iymax,
     *                          uwl2gp, vwl2gp,
     *                          wghttp, wghtpa,
     *                          ibuff(work_ptr),
     *                          s )
          call util_rdclck( smoo_cpu, smoo_elapsed )
      end if

C     Initialise aperture and beam aperture arrays.
      do 100, i = 1, aper_size*3
          rbuff(i) = 0.0
  100 continue

*      call io_wrout( 'Calculating convolution function ...' )
      call tabfn(convtp, convos, convhw, convpa, max_conv_hw, conv, s)
      if ( s .ne. 0 ) goto 9999

      string(1:)    = 'Gridding'
      call util_stclck

      do 700, i = 1, numlsf
          call enmlsf( i, file_name, lsf_key, s )
          string(11:26) = file_name
          call io_wrout( string(1:27) )

          call lsf_open( file_name, lsf_key, 'READ', lsf_num, s )
          call lsf_enq_numbuff( lsf_num, num_buff, s )
          tot_vis = 0
          tot_val = 0

C   Find the effective UV plane skew angle for this LSF
C - this also sets the phase centre of the LSF
          call get_skew_angle( lsf_num, skew_angle, s )

C  If map is a beam-set define model LSF.
          if (maptyp .eq. 3) then
C  Turn off/on bandwidth smearing and primary beam if necessary.
C    & integration-time smearing
              if (btest(bsetfw,1)) then
                  call lsf_set_bandpass( lsf_num, 1, 0.0, s )
              else
                  call lsf_set_bandpass( lsf_num, 1,
     *                                    effective_bandwidth, s )
              endif
              if (btest(bsetfw,2)) then
                  call lsf_set_pbeam( lsf_num, .false., s )
              else
                  call lsf_set_pbeam( lsf_num, .true., s )
              endif
              if (btest(bsetfw,3)) then
                  call lsf_set_integt( lsf_num, 0.0, s )
              else
                  call lsf_set_integt( lsf_num,
     *                                 effective_integration, s )
              endif
C  Add in sources.
              do 300, u = u0set, u0set+duset*(nuset-1), duset
                  do 200, v = v0set, v0set+dvset*(nvset-1), dvset
                      call uvtord( u, v, ra, dec, s )
                      call lsf_add_source(lsf_num, 1,epoch,ra,dec,1.0,s)
  200             continue
  300         continue
          end if
          if ( s .ne. 0 ) goto 9999

C Look up aerial system temperatures                      (8 May 2000)
          call lsf_enq_sf(lsf_num, slun, lsf_source, s)
          call enq_tsys  (slun, Tsys, nominal_rg, s)

C Set up table of aerial pairs for each visibility
          do 550, j = 1, max_vis
              call lsf_enq_ae_vis(lsf_num, j, ae1, ae2, s)
              samp_aes( j, 1 ) = ae1
              samp_aes( j, 2 ) = ae2
  550     continue



          do 600, j = 1, num_buff

              call lsf_set_buffer( lsf_num, j, s )
              call lsf_get_vis( lsf_num,
     *                          max_vis,
     *                          vis,
     *                          num_vis,
     *                          s )
              call lsf_get_uv( lsf_num,
     *                         max_vis,
     *                         skew_angle,
     *                         uv,
     *                         num_vis,
     *                         s )
              if ( s .ne. 0 ) goto 9999

              if (maptyp .eq. 3) then
C  Beamset - get model visibilities.
                  call lsf_set_buffer( lsf_num, j, s )
                  call lsf_get_model( lsf_num,
     *                                max_vis,
     *                                model_vis,
     *                                num_vis,
     *                                s )
              end if

C             Rescale u and v positions
              do 400 k = 1, num_vis
                  uv( 1, k ) = uv( 1, k ) * uwl2gp
                  uv( 2, k ) = uv( 2, k ) * vwl2gp
  400         continue

              call get_weight_buff(   num_vis,
     *                                vis, uv,
     *                                wghttp, wghtpa,
     *                                rbuff( smooth_ptr ),
     *                                aper_cols, iymax,
     *                                weight,
     *                                s )

              call get_rg_weight(     num_vis,
     &                                 wghttp,
     &                                 lsf_num,
     &                                 max_vis,
     &                                 samp_aes,
     &                                 Tsys, j,
     &                                 nominal_rg,
     &                                 weight,
     &                                 rg_weight,
     &                                 s )


C Eliminate null visibilities and make beams
              val_vis = 0
              do 500, k = 1, num_vis
                  if ( vis( k ) .ne. ( 0.0, 0.0 ) ) then
                      val_vis = val_vis + 1
                      uv( 1, val_vis ) = uv( 1, k )
                      uv( 2, val_vis ) = uv( 2, k )
                      weight( val_vis )= weight( k )
                      if (maptyp .eq. 1) then
                          vis(val_vis) = vis( k )
                      else if (maptyp .eq. 2) then
                          vis(val_vis) = (1.0, 0.0)
                      else
                          vis(val_vis) = model_vis( k )
                      end if
                  end if
  500         continue
              tot_vis = tot_vis + num_vis
              tot_val = tot_val + val_vis

              call grid_vis_buff( vis,
     *                            uv,
     *                            val_vis,
     *                            cbuff(aper_ptr),
     *                            aper_cols, iymax,
     *                            convhw, convos, conv, max_conv_hw,
     *                            weight,
     *                            rbuff(beam_ptr),
     *                            s )
  600     continue

          call lsf_close( lsf_num, s )
          if ( s .ne. 0 ) goto 9999

          write(*,'(X,A,I8,A,I8,A, F7.2,A)' )
     *        'Valid visibilities :', tot_val, ' out of ', tot_vis,
     *        ' - ', (100.0*real(tot_val)/tot_vis), '%.'
  700 continue

      call grade_aperture(    cbuff( aper_ptr ),
     *                        aper_cols, iymax,
     *                        uwl2gp, vwl2gp,
     *                        gradtp, gradpa,
     *                        rbuff( beam_ptr ), 1,
     *                        s )
      call util_rdclck( int_cpu, int_elapsed )

*      call io_wrout( 'Doing map fft ... ' )
      call util_stclck
      call aper2map_fft( cbuff(aper_ptr), aper_cols, iymax, s )
      call util_rdclck( fft_cpu, fft_elapsed )

C Apply the grid correction function and fft scaling correction.
*      call io_wrout( 'Correcting map ...' )
      call correct_map(   rbuff(map_ptr), ixmax, iymax,
     *                    convhw, convos, max_conv_hw, conv,
     *                    corrtp, .false., s    )

C Fill in the redtape.
      call scnmap( rbuff(map_ptr), uvmapw, stats, posns, s )
      call stscal( stats, posns, s )
      call adredt( 'Created', 'mapper', s )

C Finally write the map away...
*      call io_wrout( 'Writing the map away... ' )
      call wrredt( lun, 0, s )
      call wrmap( lun, rbuff(map_ptr), s )
      close ( lun )
      if ( s .ne. 0 ) goto 9999

      i = chr_lenb(map_name)
      call io_wrout( 'Map ' // map_name(1:i) // ' has been made' )

C Now see if a beam file exists.
      call io_brkfil( map_name, user, beam_name, type )
      call io_makfil( user, beam_name, 'beam', file_name, ls )
      call io_namfil( file_name, beam_name, 0, s )

      if (s .eq. 0 .and. maptyp.eq.1) then
C  Beam file exists, make beam.
          do 800, i = 1, aper_size
              cbuff(i) = cmplx(rbuff(beam_ptr+i-1), 0.0)
  800     continue

*          call io_wrout( 'Doing beam fft ... ' )
          call util_stclck
          call aper2map_fft( cbuff(aper_ptr), aper_cols, iymax, s )
          call util_rdclck( cpu, elapsed )
          fft_cpu     = fft_cpu     + cpu
          fft_elapsed = fft_elapsed + elapsed

C Apply the grid correction function and fft scaling correction.
*          call io_wrout( 'Correcting beam ...' )
          call correct_map(   rbuff(map_ptr), ixmax, iymax,
     *                        convhw, convos, max_conv_hw, conv,
     *                        corrtp, .false., s    )

C Fill in the redtape.
          maptyp = 2
          call scnmap( rbuff(map_ptr), uvmapw, stats, posns, s )
          call stscal( stats, posns, s )
          call adredt( 'Created', 'mapper', s )

C Finally write the map away...
*          call io_wrout( 'Writing the beam away... ' )
          call io_resfil( beam_name, nptot, .true., 0, s )
          call opemap( lun, beam_name, 'WRITE', 1, s )
          call wrredt( lun, 0, s )
          call wrmap( lun, rbuff(map_ptr), s )
          close ( lun )
          if ( s .ne. 0 ) goto 9999

          i = chr_lenb(beam_name)
          call io_wrout('Beam ' // beam_name(1:i) // ' has been made')
      else
          s = 0
      end if

C  Write profiling statistics [suppressed 25 May 2000]
*      if (smoo_cpu .ne. 0.0)
*     *write( *,2000 ) 'Smoothing ..... -', smoo_cpu, smoo_elapsed
*      write( *,2000 ) 'Gridding ...... -', int_cpu, int_elapsed
*      write( *,2000 ) 'Transforming .. -', fft_cpu, fft_elapsed
* 2000 format( 1H , 'Times for ', A, ' cpu ',F8.2, ', elapsed',F8.2 )

*      call io_wrout( ' ' )
      call io_wrout( ' ' )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine MAPPER' )
          return
      end
