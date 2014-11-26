C
C+ion_init_ion
C
      subroutine ion_init_ion( phys_sf_name, lsf_key, s )
C
C     Initialises the ionospheric correction definition to defaults.
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
C     Subroutine to construct default red tape for an ionospheric
C     correction given a logical sample file.
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
      include '/mrao/include/constants.inc'
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/lsflib_errors.inc'
      include '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Logical sample file number
              integer         lsf_num
C         Lsf information
              integer         first_samp, last_samp, num_buff
              real            max_radius
              real*8          ra, dec, lsf_epoch
              character*16    source

C     ****************************************************************
C
C     Function initialisation
C
      if ( s .ne. 0 ) goto 9999

C     Open the logical sample file to be used to make the ion.
      call lsf_open( phys_sf_name, lsf_key, 'READ', lsf_num, s )

C     Call lsf enquiry routines to find the maximum and minimum sample
C     in the lsf.
      call lsf_enq_numbuff( lsf_num, num_buff, s )
      call lsf_enq_samples( lsf_num, 1, first_samp, last_samp, s )
      call lsf_enq_samples( lsf_num, num_buff, i, last_samp, s )
      call lsf_enq_pc_epoch(lsf_num, lsf_epoch, ra, dec, source, s )
      call lsf_enq_max_rad( lsf_num, max_radius, s )

C     Close the logical sample file
      call lsf_close( lsf_num, s )
      if ( s .ne. 0 ) goto 9999

C     ****************************************************************
C
C         Main Code
C         ---------
C
      ion_number  = 0
      ion_type    = 1
      ion_key     = 0
      ion_lsf     = lsf_key
      ion_first   = first_samp
      ion_last    = last_samp
      ion_numsrcs   = 1
      ion_epoch     = lsf_epoch
      ion_source(1) = source
      ion_ra(1)     = ra
      ion_dec(1)    = dec
      do 100, i = 2, max_ionsrcs
          ion_source(i) = ' '
          ion_ra(i)     = 0.0D+0
          ion_dec(i)    = 0.0D+0
  100 continue

      do 200, i = 1, max_ionpars
          ion_param(i) = 0
  200 continue

      map_size      = max_map_size
C     Arrange for an oversampling of 25
      arcsec_per_gp = 1.0 /( 25.0 * const_sa2r * 2.0 * max_radius )
C     Search out to four beamwidths.
      search_rad    = 4.0 * 25.0 * arcsec_per_gp

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C

 9999 continue
C         Don't print error message if the error is NO_LSFSAVED or
C         USR_BREAK - let the calling program handle it.
          if (s .ne. NO_LSFSAVED .and. s .ne. USR_BREAK) then
              call ion_wrerr( s, ' in subroutine ion_init_ion ' )
          end if
          return
      end
