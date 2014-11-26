C     ******************************************************************
C
C     Remove definition record.
C
C     Remove records define removes stored in a remove file and are
C     stored at the end of the control tables of the remove file.
C
C     modified by PJW to allow 6 model point sources + flags     (10/91)
C                     and aperture interpolation type (linear or not)
C                                                                (11/91)
C     modified by DJT to allow 16 model point sources + flags    (7/92)
C
C     Other control tables items relevant to remove files are :
C
C     Section 1.  -   ISPAE, ITSCOPE, Control_tables_version_no
C     Section 2.  -   All.
C     Section 3.  -   AUNITS, AMPSCL
C     Section 4.  -   ISAMPS
C     Section 6.  -   FROBS, USPAC
C     Section 7.  -   DATOBS, NSP, INTSAM, IDAT1, IDAT2, ISTIM1, ISTIM2
C     Section 8.  -   Control tables packing record.
C
C     DJT, 3 July 92, Unix implementation
C
C     ******************************************************************

C     Remove definition.
C     ------------------

C     Remove definition length
          integer         rem_length
          parameter     ( rem_length = 168 )
C     Remove record.
          integer*4       remove_record( rem_length )

C     Remove type
          integer         rem_type
          equivalence   ( rem_type, remove_record(1) )
C     Remove date (date and time in internal format).
C      - used as a unique key when opening.
          integer         rem_key
          equivalence   ( rem_key, remove_record(2) )
C     Logical sample file describing the remove
          integer         rem_lsf
          equivalence   ( rem_lsf, remove_record(3) )
C     Ionospheric correction to use for this remove.
          integer         rem_ion
          equivalence   ( rem_ion, remove_record(4) )
C     Source name
          character*16    rem_source
          equivalence   ( rem_source, remove_record(5) )
C     Source reference date
          real*8          rem_refdat
          equivalence   ( rem_refdat, remove_record(9) )
C     Source Type (1 = point, 2 = aperture model )
          integer         rem_src_type
          equivalence   ( rem_src_type, remove_record( 11 ) )

C     Flag set to suppress the source model primary beam correction.
          logical         rem_no_pbeam
          equivalence   ( rem_no_pbeam, remove_record( 12 ) )

C     Flag to control correction of bandwidth effects
          logical         rem_no_band
          equivalence   ( rem_no_band, remove_record( 13 ) )
C     Bandwidth (Mhz)
          real            rem_bandwidth
          equivalence   ( rem_bandwidth, remove_record( 14 ) )
C     Telescope bandpass type (0 = none, 1 = boxcar, 2 =  gaussian)
          integer         rem_band_type
          equivalence   ( rem_band_type, remove_record( 15 ) )
C     Flag to control correction of integation-time effects
          logical         rem_no_integt
          equivalence   ( rem_no_integt, remove_record( 16 ) )
C     Effective integration-time (secs)
          real            rem_integt
          equivalence   ( rem_integt, remove_record( 17 ) )

C     Maximum amplitude in remove model
          real            rem_model_max
          equivalence   ( rem_model_max, remove_record( 18 ) )

C     Number of remove model point sources
          integer         rem_num_point
          equivalence   ( rem_num_point, remove_record( 19 ) )

C     Source position
          real*8          rem_ra(16), rem_dec(16)
          equivalence   ( rem_ra(1),  remove_record(21) )
          equivalence   ( rem_dec(1), remove_record(53) )

C     The following are only relevant if the remove source is a model.
C     Source flux - if an aperture model it is a relative value.
          real            rem_src_flux(16)
          equivalence   ( rem_src_flux(1), remove_record( 85 ) )

C     Aperture model file name
          character*64    rem_model_file
          equivalence   ( rem_model_file, remove_record( 101 ) )

C     Aperture interpolation type ( 1 = linear, 2 = non linear )
          integer         rem_ap_interp
          equivalence   ( rem_ap_interp, remove_record( 117 ) )

C     parameters of noise to be added to models
          real            rem_mod_mean, rem_mod_sigma
          equivalence   ( rem_mod_mean, remove_record( 118 ) )
          equivalence   ( rem_mod_sigma, remove_record( 119 ) )
