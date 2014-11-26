C     ******************************************************************
C
C     Ionospheric correction definition.
C
C     ******************************************************************


C     1.  Redtape and global constants
C     --------------------------------

C     Maximum number of points in the 1-D map made to calculate the
C     correction.
          integer         max_map_size
          parameter     ( max_map_size = 2048 )
C     Maximum number of points in the corresponding 1-D aperture.
          integer         max_aper_size
          parameter     ( max_aper_size = max_map_size/2+1 )
C     Maximum number of parameters used to describe the correction.
          integer         max_ionpars
          parameter     ( max_ionpars = 8 )

C     Maximum number of phase centre definitions included in the redtape.
C     This parameter is the dimension of the packed arrays containing
C     source names and positions.
          integer         max_ionsrcs
          parameter     ( max_ionsrcs = 8 )

C     The number of the ionospheric correction in the sample file.
C         - zero if not yet saved.
          integer         ion_number
C     Ionospheric correction type.
          integer         ion_type
C     The file key to the correction - zero if not yet made.
          integer         ion_key
C     Dummy variable to ensure alignment of real*8 values.
          real            ion_dummy
C     The epoch of the sample file and logical sample file
          real*8          ion_epoch
C     The logical sample file to use for the correction.
          integer         ion_lsf
C     The number of sources used in the correction.
          integer         ion_numsrcs
C     The sample range for the correction.
          integer         ion_first
          integer         ion_last
C     Parameters describing the correction (to be defined).
          integer         ion_param( max_ionpars )
          integer         map_size
          real            arcsec_per_gp
          real            search_rad
          real            min_amp
          real            source_size
          integer         num_iters
          real            radial_factor
          equivalence   ( ion_param(1), map_size      )
          equivalence   ( ion_param(2), arcsec_per_gp )
          equivalence   ( ion_param(3), search_rad    )
          equivalence   ( ion_param(4), min_amp       )
          equivalence   ( ion_param(5), source_size   )
          equivalence   ( ion_param(6), num_iters     )
          equivalence   ( ion_param(7), radial_factor )

C     (ii)  Phase centre information.
C     Source names.
          character*16    ion_source( max_ionsrcs )
C     Source positions at epoch of observation
          real*8          ion_ra( max_ionsrcs )
          real*8          ion_dec( max_ionsrcs )

      common   / ion_rt / ion_number, ion_type, ion_key,
     *                    ion_dummy, ion_epoch,
     *                    ion_lsf, ion_numsrcs,
     *                    ion_first, ion_last, ion_param,
     *                    ion_source, ion_ra, ion_dec
