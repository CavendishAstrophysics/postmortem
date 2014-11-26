C+CREATE_SOURCE

      subroutine create_source ( lun, src_type,
     *                           first_psf_samp, last_psf_samp,
     *                           num_samples, num_vis,
     *                           src_num, s )
C
C     Creates a new source for a given physical sample file.
C
C     Given:
C         Logical unit number of the physical sample file (PSF).
              integer         lun
C         Source type to create - either 'REM' or 'CAL'
              character*(*)   src_type
C         Sample range in the PSF for which the source is applicable.
              integer         first_psf_samp, last_psf_samp
C         Number of samples in source.
              integer         num_samples
C         Number of visibilities in source.
              integer         num_vis

C     Returned:
C         Source number of the source in the remove or calibration file
              integer     src_num
C         Status - must be zero on entry
              integer     s
C
C     Creates a new source for a physical sample file, updating the
C     remove or calibration file control tables to accommodate
C     the given number of samples. If the remove or calibration file
C     doesn't exist it is created and space is allocated for the source.
C
C     Note that the parameters describing the new source are not added
C     by this routine;  use a subsequent call to SET_SRC_DEF.
C
C     The remove or calibration file is assumed to be closed and is
C     left in this state.
C
C     S = 0 for successful return, otherwise errcode.
C
C     DJT, NPR,   October 1987.
C     30 Aug 89 - Updated to support version 2 control tables [DJT]
C-
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/ctab_pack.inc'
      include '/mrao/post/include/src_pack.inc'
      include '/mrao/post/include/sf_pack.inc'
      include '/mrao/post/include/samplib_errors.inc'
C
      integer         num_pages, i, srcfil_lun
      integer         blocksize
      integer         sid
      character*80    srcfil_name
      real*8          ra, dec
      logical         new_file

C     Check for non-zero entry status

      if ( s .ne. 0 ) return

C     Estabish whether the remove file exists.
      call enq_namfil( lun, src_type, srcfil_name, s )

      if (s .eq. 0) then
C         Remove file exists, so read control tables from it.
          new_file = .false.
          call open_sf( srcfil_lun, srcfil_name, 'READ', 0, s )
          call enq_ctab_pack( srcfil_lun, ctab_pack, s )
          call enq_src_pack( srcfil_lun, ct_num_src, src_pack, s )
          call close_sf( srcfil_lun, s )
          ct_num_src = ct_num_src+1
          src_samp_ptr = src_samp_ptr + src_num_samp*src_samp_len
      else if ( s .eq. NO_FILE ) then
C         Remove file does not exist so fabricate new control tables
C         from source physical sample file.
          s = 0
          new_file = .true.
          call read_ct( lun, s )

C         Initialise
          if (ct_vers.le.1) then
              ct_vers  = 1
              ct_pages = 6
              ct_pack_type = 1
              ct_src_ptr = ctv1_var_ptr + ctab_pack_len
              do 10, i = ctv1_var_ptr+1, ct_pages*page
                  ct_all(i) = 0
  10          continue
          else if (ct_vers .eq.2) then
              ct_pages = 8
              ct_pack_type = 2
              ct_src_ptr = ctv2_var_ptr
              do 11, i = ctv2_var_ptr+1, ct_pages*page
                  ct_all(i) = 0
  11          continue
          end if

C         Create new control tables packing information.
          if ( chr_cmatch( src_type, 'REM' ) ) then
              ct_type     = 2
              ct_len_src  = 200
          else
              ct_type     = 3
              ct_len_src  = 200
          end if

          ct_pack_len  = ctab_pack_len
          ct_sf_saved  = .false.
          ct_num_src   = 1
          ct_len_pack  = src_pack_len
          ct_max_src   = (ct_pages*page-ct_src_ptr)/ct_len_src
          src_samp_ptr = ct_pages*page

      end if

C     Get the physical sample file packing parameters for amp. factor.
      call get_sf_pack( lun, 1, s )
      if ( s .ne. 0 ) goto 999

C     Create new source packing information.
      src_pack_type   = 1
C     src_samp_ptr    = already set up.
      src_num_samp    = num_samples
      src_samp_len    = num_vis+8
      src_start_rt    = 1
      src_length_rt   = 8
      src_start_vis   = src_start_rt+src_length_rt
      src_max_vis     = num_vis
      src_data_type   = 1
      src_amp_factor  = amp_factor
      if ( chr_cmatch( src_type, 'CAL' ) ) src_amp_factor=32767.0/10.0
      src_start_mon   = 0
      src_length_mon  = 0
      call read_rt( lun, 1, first_psf_samp, ra, dec, sid, s )
      src_start_time = sid-1
      call read_rt( lun, 1, last_psf_samp, ra, dec, sid, s )
      src_stop_time  = sid+1
      src_interp_type = 1
C  Use linear interpolation as default for RT (DJT, 24/11/92)
      if ( ct_vers.eq.2 ) src_interp_type = 2
      if ( chr_cmatch( src_type, 'REM' ) ) src_interp_type = 2

      src_num   = ct_num_src
      num_pages = (src_samp_ptr+src_num_samp*src_samp_len-1)/page + 1

C     Now reserve space, then write the control tables away.
      call io_resfil( srcfil_name, num_pages, .false., 1, s )
      if (s .ne. 0) goto 999

      if ( new_file ) then
C         This is the one place where a sample file is opened without
C         using open_sf - because the control tables aren't valid yet.
          blocksize = (page*4)
          call io_operan( srcfil_lun, srcfil_name, 'WRITE',
     :                                         blocksize, 0, s )
          ct_lun = srcfil_lun
      else
          call open_sf( srcfil_lun, srcfil_name, 'WRITE', 0, s )
      end if

      call set_ctab_pack( srcfil_lun, ctab_pack, s )
      call set_src_pack( srcfil_lun, ct_num_src, src_pack, s )
      call close_sf( srcfil_lun, s )
      if (s .ne. 0) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine CREATE_SOURCE' )

      end
