C     ******************************************************************
C
C     Ionospheric correction definition.
C
C     An ionospheric correction file consists of two pages of redtape,
C     describing the correction data which is stored with the visibility
C     data in the physical sample file.
C
C     Each correction defined in the redtape applies to a specified range
C     of samples, and is identified within the data by the index ion_index.
C
C     Each correction may be made with respect to several phase centres.
C     These are identified within the packed arrays of source names and
C     positions via the parameter ion_srcs.
C
C     The correction data is held within each sample buffer, after the
C     visibility data and the monitor block.  Up to 32 corrections can
C     in principle be held, but this may be reduced depending on the
C     space available in the specific sample file.
C
C     ******************************************************************


C 1.  Redtape and global constants
C --------------------------------

          integer           ion_length
          parameter       ( ion_length = 1024 )
          integer           ion_redtape( ion_length )
          common /ion_corr/ ion_redtape

C     Maximum number of corrections, size of tables within redtape.
          integer       max_corrs
          parameter   ( max_corrs = 32 )

C     Maximum number of parameters used to describe the correction.
          integer       max_ionpars
          parameter   ( max_ionpars = 8 )

C     Maximum number of phase centre definitions included in the redtape.
C     This parameter is the dimension of the packed arrays containing
C     source names and positions.
          integer       max_ionsrcs
          parameter   ( max_ionsrcs = max_corrs*2 )


C 2.  General information about the corrections.
C ----------------------------------------------

C     Number of corrections defined within redtape.
          integer       num_corrs
          equivalence ( num_corrs, ion_redtape(1) )

C     Number of old-style corrections
C         - these are stored as imitation visibility data.
          integer       num_old_corrs
          equivalence ( num_old_corrs, ion_redtape(2) )

C     Ionospheric correction scaling factor - for old corrections.
          real*4        ion_factor
          equivalence ( ion_factor, ion_redtape(3) )

C     Maximum number of new corrections within each data sample.
          integer       max_index
          equivalence ( max_index, ion_redtape(4) )


C 3.  Information about each correction.
C --------------------------------------


C     (i)    Packing information on storage of new style corrections.

C     The index of the correction within each data sample.
          integer       ion_index( max_corrs )
          equivalence ( ion_index, ion_redtape(5) )

C     The sample range for each correction.
          integer       ion_first( max_corrs )
          integer       ion_last( max_corrs )
          equivalence ( ion_first, ion_redtape(max_corrs*1+5) )
          equivalence ( ion_last,  ion_redtape(max_corrs*2+5) )


C     (ii)   Correction calculation information.

C     Ionospheric correction type - old style corrections are type 0
          integer       ion_type( max_corrs )
          equivalence ( ion_type, ion_redtape(max_corrs*3+5) )

C     Key for accessing the correction (date/time in internal format).
C       - a zero indicates correction has not yet been made.
          integer       ion_key( max_corrs )
          equivalence ( ion_key, ion_redtape(max_corrs*4+5) )

C     The logical sample file to use for the correction.
          integer       ion_lsf( max_corrs )
          equivalence ( ion_lsf, ion_redtape(max_corrs*5+5) )

C     The number of sources used in the correction.
          integer       ion_srcs( max_corrs )
          equivalence ( ion_srcs, ion_redtape(max_corrs*6+5) )

C     Parameters describing the correction (to be defined).
          integer       ion_param( max_ionpars, max_corrs )
          equivalence ( ion_param, ion_redtape(max_corrs*7+5) )


C     (iii)  Phase centre information.

C     Offset pointer for source definitions
          integer       ion_srcptr
          parameter   ( ion_srcptr = max_corrs*(max_ionpars+7)+5 )

C     Source names.
          character*16  ion_source( max_ionsrcs )
          equivalence ( ion_source, ion_redtape(ion_srcptr) )

C     Source positions at epoch of observation
          real*8        ion_ra( max_ionsrcs )
          real*8        ion_dec( max_ionsrcs )
          equivalence ( ion_ra,  ion_redtape(max_ionsrcs*4+ion_srcptr) )
          equivalence ( ion_dec, ion_redtape(max_ionsrcs*6+ion_srcptr) )


C     Total length of redtape = max_ionsrcs*8+ion_srcptr-1 = 996 words
C                                                            =========


C     2.  Data block
C     --------------

C     Array containing correction data for each correction.
          integer       ion_block( 4 )

C     Correction magnitude - preferably stored as radians/wavelength.
          real          ion_phase
          equivalence ( ion_phase, ion_block(1) )

C     Second order term - baseline scaling expressed as fractional
C     stretching (delta l/l).
          real          ion_scale
          equivalence ( ion_scale, ion_block(2) )

C     Processing information - detected total source amplitude.
          real          ion_amp
          equivalence ( ion_amp, ion_block(3) )

C     Processing information - probable correction error.
          real          ion_err
          equivalence ( ion_err, ion_block(4) )


C     Total length of data block = ion_blklen = 65 words
C                                               ========
