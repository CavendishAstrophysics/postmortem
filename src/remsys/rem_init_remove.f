C
C+rem_init_remove
C
      subroutine rem_init_remove( phys_sf_name, lsf_key, s )
C
C     Initialises all the remove definition to reasonable defaults.
C
C     Given:
C         Physical sample file name
              character*(*)   phys_sf_name
C         Logical sample file key
              integer         lsf_key

C     Returned:
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
C         Other       - Unexpected system error
C-
C     ****************************************************************
C
C     Function declarations
C
      include '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Common block includes - ( i.e. Global variable declaration. )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/remsys_errors.inc'
      include '/mrao/post/include/remove_common.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Logical sample file number
              integer         lsf_num
C         Lsf information
              integer         ion
              real*8          ra, dec, refdat
              character*16    source
C         Physical sample file source, unit number
              integer         sf_lun, src_num
C         LSF smoothing paramaters
              integer         sm_type, sm_size, samp_rate

C     ****************************************************************
C
C     Function initialisation
C
      if ( s .ne. 0 ) goto 9999

C     Open the logical sample file to be used to make the remove.
      call lsf_open( phys_sf_name, lsf_key, 'READ', lsf_num, s )
      if ( s .ne. 0 ) goto 9999

C     Call control table enquiry routines.
      call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
      call lsf_enq_pc_rdate( lsf_num, refdat, ra, dec, source, s )
      call lsf_enq_ion( lsf_num, ion, s )
      call lsf_enq_smooth( lsf_num, sm_type, sm_size, samp_rate, s )

C     Close the logical sample file
      i = 0
      call lsf_close( lsf_num, i )
      if ( s .eq. 0 .and. i .ne. 0 ) s = i
      if ( s .ne. 0 ) goto 9999

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Initialise the remove number and the remove record.
      rem_number    = 0
      do 100, i = 1, rem_length
          remove_record(i) = 0
  100 continue

C      rem_type   = 0
C      if (sm_type .gt. 1) rem_type = 1
      rem_type = 1
      rem_key    = 0
      rem_lsf    = lsf_key
      rem_ion    = ion
      rem_source = source
      rem_refdat = refdat
      rem_ra(1)  = ra
      rem_dec(1) = dec
      rem_num_point = 1
      rem_model_file = ' '
      rem_no_pbeam = .true.
      rem_no_band = .true.
      rem_bandwidth = 0.
      rem_no_integt = .true.
      rem_integt = 0.
      rem_mod_mean = 0.
      rem_mod_sigma = 0.

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 9999 continue
C         Don't print error message if the error is NO_LSFSAVED
C         Let the calling program handle it.
          if ( s .ne. NO_LSFSAVED) then
              call rem_wrerr( s, 'in subroutine REM_INIT_REMOVE' )
          end if
          return
      end
