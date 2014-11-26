
C     *****************************************************************
C
C+cal_open_lsf
C
      SUBROUTINE cal_open_lsf( access, lsf_num, s )

C     Opens the lsf and sets up the model for the current calibration.
C
C     Given:
C         Sample file open access code.
              character*(*)       access
C
C     Returned:
C         The logical sample file number.
              integer             lsf_num
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The calibration definition is obtained from the calibration
C     common blocks.
C
C-
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/cal_common.inc'

C
C     Variables, equivalences and commons
C         General purpose loop counter
              integer         i
C         Physical sample file name
              character*80    psf_name

C
C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C
C     Main Code
C     ---------
C
      psf_name = cal_sf
      call lsf_open( psf_name, cal_lsf, access, lsf_num, s )
      call lsf_set_pc( lsf_num, cal_refdat,
     *                 cal_ra(1),cal_dec(1),cal_source, s)
      call lsf_set_bandpass( lsf_num, cal_band_type, cal_bandwidth, s )
      call lsf_set_integt( lsf_num, cal_integt, s )
      call lsf_set_pbeam( lsf_num, .not.cal_no_pbeam, s )
      call lsf_set_noise( lsf_num, cal_mod_mean, cal_mod_sigma, s )

      do i = 1, max( cal_num_point, 1 )
          call lsf_add_source( lsf_num, cal_src_type, cal_refdat,
     *                       cal_ra(i), cal_dec(i), cal_src_flux(i), s)
      enddo
      if (cal_src_type.ne.1)
     *           call lsf_set_model( cal_model_file, cal_ap_interp,
     *                               cal_mod_mean, cal_mod_sigma, s )

      if ( s .ne. 0 ) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr( s, 'in subroutine cal_open_lsf ' )
          return
      end
