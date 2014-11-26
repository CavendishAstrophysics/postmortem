C
C+cal_sel_model
C
      SUBROUTINE cal_sel_model( s )

C     Asks the user to select the model in the calibration record.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'
      include  '/mrao/post/include/merge_types.inc'

C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Buffer for current values of cal_record
              integer         buffer( cal_length )
C         Logical sample file number and sample file unit number
              integer         lsf_num, sf_lun
C         Flag set if calibration model file exists
              logical         exist
C         Source type
              character*14    reply
C         Valid source types
              character*14    model_types(2)
              data  model_types / 'point', 'model-aperture' /
C         to help set up the default user for :aper file
              character       map_dir*24
              integer         ls

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      do 100, i = 1, cal_length
          buffer(i) = cal_record(i)
  100 continue

C
C     Main Code
C     ---------
C
      call io_getopt( 'Model type (?=list) : ',
     *              model_types(cal_src_type),
     *              model_types, 2,
     *              reply, s      )

      if ( chr_cmatch( reply, 'model-aperture' ) ) then
          cal_src_type   = 2
  200     continue
              call enmdir( map_dir, s )
              call io_makfil( map_dir, '', 'aper', cal_model_file, ls)
              call io_getfil( 'Model file name: ',
     *                     '*', cal_model_file, s )
              if (s .ne. 0) goto 9999
              inquire( file = cal_model_file, exist=exist )
              if (.not.exist) call io_wrout( '*** File does not exist' )
          if (.not.exist) goto 200
          call io_getr( 'Factor to multiply model by: ',
     *               '*', cal_src_flux(1), s )
          if ( io_yesno('Use linear interpolation on aperture-model ?',
     *                    'yes', s ) ) then
              cal_ap_interp = 1
          else
              cal_ap_interp = 3
          endif
      else if ( s .eq. 0 ) then
          cal_src_type = 1
          cal_num_point = 1
          call io_geti( 'number of point sources in model (max 6): ',
     *               '*', cal_num_point, s )
          i = 1
          do while ( i .le. cal_num_point  .and.  i .le. 6)
              call io_getra( 'RA: ', '*', cal_ra(i), s )
              call io_getdec( 'DEC: ', '*', cal_dec(i), s )
              call io_getr( 'Flux of point source in Jansky : ',
     *                   '*', cal_src_flux(i), s)
              i = i+1
          enddo
      end if

      if (io_yesno( 'Apply primary beam correction?', 'no', s ) ) then
          cal_no_pbeam = .false.
      else
          cal_no_pbeam = .true.
      end if

      if (io_yesno( 'Apply bandwidth correction?', 'no', s ) ) then
          cal_no_band = .false.
          call io_getr( ' bandwidth (MHz): ', '*', cal_bandwidth, s )
      else
          cal_no_band = .true.
          cal_bandwidth = 0.
      endif

      if (io_yesno( 'Apply integration-time correction?', 'no',
     *                                                    s ) ) then
          cal_no_integt = .false.
          call io_getr( 'Effective integration-time (secs):', '*',
     *                cal_integt, s )
      else
          cal_no_integt = .true.
          cal_integt = 0.
      endif

      if ( io_yesno( 'Add noise?', 'no', s ) ) then
          call io_getr( 'mean noise (Jy):', '*', cal_mod_mean, s )
          call io_getr( 'sigma of noise (Jy):', '*', cal_mod_sigma, s )
      else
          cal_mod_mean = 0.
          cal_mod_sigma = 0.
      endif

      if (s .ne. 0) goto 9999

      call cal_open_lsf( 'READ', lsf_num, s )
      call lsf_enq_sf( lsf_num, sf_lun, i, s )
      call get_merge( sf_lun, 'Merge type: ', '*',cal_merge_type,s)
  300 if ((cal_merge_type .eq. no_merge)      .or.
     *    (cal_merge_type .eq. subband_merge) .or.
     *    (cal_merge_type .eq. channel_merge) .or.
     *    (cal_merge_type .eq. aerial_merge)  .or.
     *    (cal_merge_type .eq. hut_merge)     .or. (s.ne.0) ) goto 400
          call io_wrout(
     *    '*** Merge must be none, aerial, hut, channel or sub-band' )
          call get_merge( sf_lun, 'Merge type: ',
     *                    '*', cal_merge_type,s)
      goto 300
 400  continue
      if ( (cal_merge_type.eq.aerial_merge) .or.
     *     (cal_merge_type.eq.hut_merge)        ) then
C .. CLFST form of factorisation
        cal_type = 1
      else
C .. factorisation by solving over-determined problem
C    reset cal_type if necessary
        if (cal_type.eq.1) then
           cal_type = 2
        end if
      end if

      i = 0
      call lsf_close( lsf_num, i )
      if (s.eq.0 .and. i.ne.0) s = i

      if (s.ne.0) goto 9999
      call io_wrout( ' ')
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call cal_wrerr( s, 'in subroutine cal_sel_model ' )
          end if
          do 9000, i = 1, cal_length
              cal_record(i) = buffer(i)
 9000     continue
          return
      end
