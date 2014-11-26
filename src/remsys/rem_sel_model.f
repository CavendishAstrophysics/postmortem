C
C+rem_sel_model
C
      SUBROUTINE rem_sel_model( s )

C     Asks the user to select the model in the remove record.
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
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/remove_common.inc'
      include  '/mrao/post/include/merge_types.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Buffer for current values of remove_record
              integer         buffer( rem_length )
C         Flag set if remove model file exists
              logical         exist
C         Source type
              character*14    reply
C         Valid source types
              character*14    model_types(3)
              data  model_types / 'point', 'model-aperture','none' /
C     local variables to help set the default :aper file name
              character       map_dir*24
              integer         ls
C     local logical to establish which correction to apply
              logical         apply

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      do 100, i = 1, rem_length
          buffer(i) = remove_record(i)
  100 continue

C     ****************************************************************
C
C         Main Code
C         ---------
C
      rem_type = 2
      call io_getopt( 'Model type (?=list) :',
     *                 model_types(rem_src_type),
     *                 model_types, 3,
     *                 reply, s      )

      if ( chr_cmatch( reply, 'model-aperture' ) ) then
          rem_src_type   = 2
  200     continue
          call enmdir( map_dir, s)
          call io_makfil( map_dir, '', 'aper', rem_model_file, ls)
          call io_getfil( 'Model file name :', '*', rem_model_file, s )
          if (s .ne. 0) goto 9999
          inquire( file = rem_model_file, exist=exist )
          if (.not.exist) then
              call io_wrout( '*** File does not exist' )
              goto 200
          endif
          rem_src_flux(1) = 1.
          call io_getr( 'Factor to multiply model by:', '*',
     *                rem_src_flux(1), s )
          if ( io_yesno( 'Use linear interpolation on aperture-model ?',
     *                    'yes', s ) ) then
              rem_ap_interp = 1
          else
              rem_ap_interp = 3
          endif
          call io_getr( 'total flux of model (before scaling):', '*',
     *                rem_model_max,  s )
      elseif ( chr_cmatch( reply, 'point' ) ) then
          rem_src_type = 1
          call io_geti( 'Number of point sources in model (max 6):',
     *                 '*', rem_num_point, s )
          i = 1
          do while ( i.le.rem_num_point .and. i.le.6)
              call io_getra( 'RA:', '*', rem_ra(i), s )
              call io_getdec( 'DEC:', '*', rem_dec(i), s )
              call io_getr( 'Flux of point source (Jansky):',
     *                '*', rem_src_flux(i), s)
              i = i + 1
          enddo
      elseif ( chr_cmatch( reply, 'none' ) ) then
          rem_type = 1
          return
      end if

      apply = .not. rem_no_pbeam
      call io_getl( 'Apply primary beam correction ?', '*', apply, s )
      rem_no_pbeam = .not. apply

      apply = .not. rem_no_band
      call io_getl( 'Apply bandwidth correction ?', '*', apply, s )
      if( apply ) then
          rem_no_band = .false.
          call io_getr( ' bandwidth (MHz): ', '*', rem_bandwidth, s )
      else
          rem_no_band = .true.
          rem_bandwidth = 0.
      endif

      apply = .not. rem_no_integt
      call io_getl( 'Apply integration-time correction ?', '*',
     *                apply, s )
      if ( apply ) then
          rem_no_integt = .false.
          call io_getr( 'Effective integration-time (secs):', '*',
     *                rem_integt, s )
      else
          rem_no_integt = .true.
          rem_integt = 0.
      endif

      if ( io_yesno( 'add noise ?', 'no', s ) ) then
          call io_getr( 'mean noise (Jy):', '*', rem_mod_mean, s )
          call io_getr( 'sigma of noise (Jy):', '*', rem_mod_sigma, s )
      else
          rem_mod_mean = 0.
          rem_mod_sigma = 0.
      endif

      if (s .ne. 0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s .ne. USR_BREAK) then
              call rem_wrerr( s, 'in subroutine REM_SEL_MODEL ' )
          end if
          do 9000, i = 1, rem_length
              remove_record(i) = buffer(i)
 9000     continue
          return
      end
