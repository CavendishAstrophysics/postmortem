C     *****************************************************************
C
C     Logical Sample File Model Definition Common Block.
C     --------------------------------------------------
C
C     This common block, and the routines that go with it, define model
C     visibility data that can be associated with a logical sample file.
C     The model consists of a number of sources (maximum 100 at present)
C     and the appropriate primary beam correction and bandwidth
C     and integration-time
C     smearing. The sources can either be point sources of a given flux,
C     or be derived from a model, supplied in the form of a model
C     aperture in the standard maplib format.
C
C     The model definition is not saved with the LSF, but must
C     be set each time it is used.
C
C     *****************************************************************

      include '(library)maplib_minirt.inc'

C     The maximum number of model sources allowed.
      integer                 mod_max_srcs
      parameter             ( mod_max_srcs = 100 )

C     The maximum size for the extended source definition buffer.
      integer                 mod_max_data
      parameter             ( mod_max_data = 257*512*2 )

C     Telescope type and pointing centre for primary beam correction.
      integer                 mod_tscope
      real*8                  mod_ra_aes, mod_dec_aes

C     Bandwidth type and fractional bandwidth for bandwidth smearing.
      integer                 mod_band_type
      real                    mod_band_width

C     Logical set if primary beam is switched on.
      logical                 mod_pb_flg

C     integration-time smearing flag and effective time(secs)
      logical                 mod_integ_flg
      real                    mod_integ_time

C     List of point source fluxes and positions and types.
      integer                 mod_num_srcs
      real                    mod_flux(mod_max_srcs)
      real*8                  mod_ra(mod_max_srcs),mod_dec(mod_max_srcs)
      integer                 mod_type(mod_max_srcs)

C     Aperture definition for non point source.
      integer                 mod_minirt(minirt_len)
      real                    mod_u_wl2gp, mod_v_wl2gp
      real*4                  mod_aper( mod_max_data )


      common       / lsf_model /
     *    mod_tscope, mod_ra_aes, mod_dec_aes,
     *    mod_band_type, mod_band_width, mod_pb_flg,
     *    mod_integ_flg, mod_integ_time,
     *    mod_num_srcs, mod_flux, mod_ra, mod_dec, mod_type,
     *    mod_minirt, mod_u_wl2gp, mod_v_wl2gp, mod_aper
