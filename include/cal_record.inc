C
C Calibration (gains) definition record
C -------------------------------------
C
C     Calibration definition records define calibrations stored in a
C     :CAL file and are stored at the end of the control tables of the
C     :CAL file.
C
C     modified by PJW to hold 6 model sources + flags          10/91
C                    and linear/non interpolation of apertures 11/91
C
C     Other control tables items relevant to :CAL files are :
C
C     Section 1.  -   ISPAE, ITSCOPE, ct_vers
C     Section 2.  -   All.
C     Section 3.  -   AUNITS, AMPSCL
C     Section 4.  -   ISAMPS
C     Section 6.  -   FROBS, USPAC
C     Section 7.  -   DATOBS, NSP, INTSAM, IDAT1, IDAT2, ISTIM1, ISTIM2
C     Section 8.  -   Control tables packing record.
C
C     ******************************************************************

C Calibration definition.
C -----------------------

C     Calibration definition length
          integer         cal_length
          parameter     ( cal_length = 168 )
C     Calibration record.
          integer*4       cal_record( cal_length )

C     Calibration type
          integer         cal_type
          equivalence   ( cal_type, cal_record(1) )
C     Calibration creation date (date and time in internal format).
C      - used as a unique key when opening.
          integer         cal_key
          equivalence   ( cal_key, cal_record(2) )
C     Sample file calibration was derived from.
          character*16    cal_sf
          equivalence   ( cal_sf, cal_record(3) )
C     Logical sample file describing the calibration.
          integer         cal_lsf
          equivalence   ( cal_lsf, cal_record(7) )
C     Calibration source name
          character*16    cal_source
          equivalence   ( cal_source, cal_record(8) )
C     Source reference date
          real*8          cal_refdat
          equivalence   ( cal_refdat, cal_record(13) )

C     Source Type (1 = point, 2 = aperture model )
          integer         cal_src_type
          equivalence   ( cal_src_type, cal_record( 15 ) )

C     Flags set to suppress amplitude or phase part of the correction
          logical         cal_no_amp, cal_no_phi
          equivalence   ( cal_no_amp, cal_record( 16 ) )
          equivalence   ( cal_no_phi, cal_record( 17 ) )

C     Flag set to suppress the source model primary beam correction.
          logical         cal_no_pbeam
          equivalence   ( cal_no_pbeam, cal_record( 18 ) )

C     Sample merge type to use - see merge_types.inc
          integer         cal_merge_type
          equivalence   ( cal_merge_type, cal_record( 19 ) )

C     Telescope bandwidth in Hz.
          real            cal_bandwidth
          equivalence   ( cal_bandwidth, cal_record( 20 ) )

C     Telescope bandpass type (0=none, 1=boxcar, 2=gaussian)
          integer         cal_band_type
          equivalence   ( cal_band_type, cal_record( 21 ) )

C     Reference antenna
          integer         cal_refant
          equivalence   ( cal_refant, cal_record( 22 ) )

C     Flag to control bandwidth correction
          logical         cal_no_band
          equivalence   ( cal_no_band, cal_record( 23 ) )

C     Flag to control integration-time correction
          logical         cal_no_integt
          equivalence   ( cal_no_integt, cal_record( 24 ) )

C     Effective integration-time (secs)
          real*4          cal_integt
          equivalence   ( cal_integt, cal_record( 25 ) )

C     Number of calibration point sources
          integer         cal_num_point
          equivalence   ( cal_num_point, cal_record( 26 ) )

C     Source position
          real*8          cal_ra(6), cal_dec(6)
          equivalence   ( cal_ra(1),  cal_record(27) )
          equivalence   ( cal_dec(1), cal_record(39) )

C     Source flux - if an aperture model it is a relative value.
          real            cal_src_flux(6)
          equivalence   ( cal_src_flux(1), cal_record( 51 ) )

C     Aperture model file name
          character*64    cal_model_file
          equivalence   ( cal_model_file, cal_record( 57 ) )

C     Aperture interpolation type ( 1 = linear, 2 = nonlinear )
          integer         cal_ap_interp
          equivalence   ( cal_ap_interp, cal_record( 73 ) )

C     parameters of noise to add to models  - mean and sigma
          real            cal_mod_mean, cal_mod_sigma
          equivalence   ( cal_mod_mean, cal_record( 74 ) )
          equivalence   ( cal_mod_sigma, cal_record( 75 ) )
