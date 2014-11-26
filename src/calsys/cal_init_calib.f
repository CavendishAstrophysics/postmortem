C
C+cal_init_calib
C
      subroutine cal_init_calib( phys_sf_name, lsf_key, ncom_current, s)
C
C     Initialises the calibration definition to reasonable defaults.
C
C     Given:
C         Physical sample file name
              character*(*)   phys_sf_name
C         Logical sample file key
              integer         lsf_key

C     Returned:
C         Number of current commands
              integer         ncom_current
C         Status value, zero if success - must be zero on entry.
              integer         s
C
C     Subroutine to construct default red tape for a map file using
C     the single logical sample file specified.
C
C     Possible status values returned :
C         0           - Success.
C         NO_LSFSAVED - No logical sample files saved for this sample
C                       file - no error is logged as this should be
C                       handled by the calling programs.
C         Other       - Unexpected io_system error
C-
C
C     Function declarations
C
      include '/mrao/include/chrlib_functions.inc'

C
C     Common block includes - ( i.e. Global variable declaration. )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/calib_errors.inc'
      include '/mrao/post/include/calib_commands.inc'
      include '/mrao/post/include/cal_common.inc'
      include '/mrao/post/include/merge_types.inc'
      include '/mrao/post/include/phys_tscopes.inc'

C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         String indexes
              integer         i1, i2
C         Logical sample file number
              integer         lsf_num
C         Lsf information
              integer         num_buff
              real*8          ra, dec, refdat
              character*16    source
C         Physical sample file unit number and source number
              integer         sf_lun, src_num
C         Telescope type
              integer         tscope_code

C     ==================================================================
C
C     Function initialisation
C     -----------------------
C
      if ( s .ne. 0 ) goto 9999

C     Open the logical sample file to be used to make the calibration.
      call lsf_open( phys_sf_name, lsf_key, 'READ', lsf_num, s )
      if ( s .ne. 0 ) goto 9999

C     Call control table enquiry routines.
      call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
      call lsf_enq_pc_rdate( lsf_num, refdat, ra, dec, source, s )
      call lsf_enq_numbuff( lsf_num, num_buff, s )

C     Enquire the telescope type
      call enq_phys_tscope( sf_lun, tscope_code, s )

C     Close the logical sample file
      i = 0
      call lsf_close( lsf_num, s )
      if ( s.eq.0 .and. i.ne.0) s = i
      if ( s .ne. 0 ) goto 9999

C
C     Main Code
C     ---------
C
C     Initialise the calibration number
      cal_number    = 0

      do 100, i = 1, cal_length
          cal_record(i) = 0
  100 continue

      i2 = chr_ilstc( phys_sf_name, '/') -1
      i1 = chr_ilstc( phys_sf_name(1:i2), '/') + 1

      cal_key     = 0
      cal_sf      = phys_sf_name(i1:i2)
      cal_lsf     = lsf_key
      cal_source  = source
      cal_refdat  = refdat
      cal_num_point = 1
      cal_ra(1)   = ra
      cal_dec(1)  = dec
      cal_src_flux(1) = 1.0
      cal_src_type= 1
      cal_model_file = ' '
      cal_no_amp     = .true.
      cal_no_phi     = .false.
      if (tscope_code.eq.ryle) then
        cal_type = 2
        cal_no_amp     = .false.
        cal_no_pbeam   = .true.
        cal_merge_type = no_merge
        cal_refant = 5
        ncom_current = num_cmds
      else
        cal_type = 1
        cal_no_pbeam   = .true.
        cal_merge_type = aerial_merge
        ncom_current = num_CLFST_cmds
      end if
      cal_no_band = .true.
      cal_bandwidth  = 0.0
      cal_band_type  = 1
      cal_no_integt = .true.
      cal_integt = 0.0
      cal_mod_mean = 0.
      cal_mod_sigma = 0.

      return

C
C     Error Handling
C     --------------
C

 9999 continue
C         Don't print error message if the error is NO_LSFSAVED
C         Let the calling program handle it.
          if ( .not. ( s.eq.NO_LSFSAVED .or.
     *                 s.eq.USR_BREAK        ) ) then
              call cal_wrerr( s, 'in subroutine cal_init_calib ' )
          end if
          return
      end
