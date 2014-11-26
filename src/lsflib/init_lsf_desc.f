C
C+init_lsf_desc
C
      SUBROUTINE init_lsf_desc(  sf_lun, s )

C     Initialises the logical sample file description.
C
C     Given:
C         Open, physical sample file unit number.
              integer             sf_lun

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Initialises the logical sample file description to the default
C     values for the given physical sample file. If the physical
C     sample file has more than one source then the source number
C     is prompted for.
C
C-
C     ****************************************************************
C
C     Function definitions

      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'

C     ****************************************************************
C
C     Local variable declarations
C         Loop counter
              integer         i
C         String length
              integer         ls
C         Sample file type of source sample file
              integer         sf_type
C         Number of sources in the sample file
              integer         num_srcs
C         Source number and name.
              integer         source_number
              character*20    source_name
C         Number of spacings and samples in the sample file.
              integer         num_sp, num_samp
C         User information
              character*16    user
              integer*4       mode, term_no

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Ascertain and open the source number.
      call io_enqexe( user, mode, term_no )
      call enq_numsrc( sf_lun, num_srcs, s )
      if (num_srcs .eq. 1) then
          source_number = 1
      else if (num_srcs.gt.0.and.mode .eq. 0) then
          source_number = 0
  100     continue
             s = 0
             source_number = source_number + 1
             if (source_number .gt. num_srcs) then
               call io_wrout( '' )
               call io_wrout( 'No more sources - one must be selected.')
               call io_wrout( '' )
               source_number = 1
             end if
             call enq_src_name( sf_lun, source_number, source_name, s )
             ls = chr_lenb( source_name )
          if ((.not.io_yesno('Source '//source_name(1:ls)//' ?','No',s))
     *                                    .and. (s .eq. 0) ) goto 100
          if ( s .eq. USR_BREAK ) return
          call io_wrout( '' )
      else
          s = ILL_LSF
          goto 9999
      end if
      call open_source( sf_lun, source_number, 0, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C

C     Initialise LSF decription - lsf_key is -1 and lsf_name is null
C     to indicate the file is the default lsf.

      lsf_key       = -1
      lsf_version   = 1
      lsf_name      = ' '
      lsf_owner     = user
      call util_enqnow( lsf_time_created )
      lsf_last_user = user
      lsf_last_used = lsf_time_created

      src_num       = source_number

      pre_int_chop_type = 0
      do 200, i = 1, 10
          pre_int_chop_params(i) = 0
  200 continue

      num_removes = 0
      do 300, i = 1, max_rems
          rem_flag(i)    = 0
          lsf_rem_key(i) = 0
  300 continue

      flag_flag   = 0
      ion_flag    = 0
      lsf_ion_key = 0

      cal_flag    = 0
      lsf_cal_key = 0

      call enq_sftype( sf_lun, sf_type, s )
      if (sf_type .eq. 1 .or. sf_type .eq. 2) then
C         Remove file or Physical sample file -has a phase centre
          call enq_pc_epoch( sf_lun, src_num,
     *                       ref_date, ref_ra, ref_dec, source_text, s )
          if (ref_ra .le. 0) then
C             Pointing is an hour angle-set default phase centre to N-P.
              ref_ra      = const_pi
              ref_dec     = const_piby2
              source_text = 'North-Pole'
          else
              call chr_chucas( source_text )
          end if
      else
C         Calibration file, no phase centre but set it to the North-pole
          call enq_epoch( sf_lun, ref_date, s )
          ref_ra      = const_pi
          ref_dec     = const_piby2
          source_text = 'North-Pole'
      end if

      post_int_chop_type  = 0
      do 400, i = 1, 10
          post_int_chop_params(i) = 0
  400 continue

      smooth_type         = 0
      smooth_size         = 0
      samp_rate           = 1

      call enq_numvis( sf_lun, num_sp, s )
      call util_setbfd( sp_bit_arr, 1, num_sp )
      call util_clrbfd( sp_bit_arr, num_sp+1, max_sp )

      call enq_numsamp( sf_lun, src_num, num_samp, s )
      call util_setbfd( samp_list, 1, num_samp )
      call util_clrbfd( samp_list, num_samp+1, max_samp )

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine INIT_LSF_DESC' )
          return
      end
