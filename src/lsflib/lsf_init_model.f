
C     *****************************************************************
C

C
C$(7) Routines for defining and reading model visibilities from the LSF.
C
C+lsf_init_model
C
      SUBROUTINE lsf_init_model( lsf_num,
     *                           s                      )

C
C     Initialises the logical sample file model definition common block.
C
C     Given:
C         Current logical sample file number
              integer         lsf_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Initialises the lsf-model common blocks to the default model.
C
C     NPR     8 April 1988
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Local variable declarations
C         Loop counter
              integer         i
C         Physical sample file unit number and LSF source.
              integer         phys_sf_lun, src_num
C         General purpose string and length
              character*20    string
C         Telescope frequency
              real*8          frequency

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Get physical sample file unit number
      call lsf_enq_sf( lsf_num, phys_sf_lun, src_num, s )

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Set up model definition
      call enq_point( phys_sf_lun, mod_ra_aes, mod_dec_aes, s )
      call enq_tscope( phys_sf_lun, string, mod_tscope, s )
      call enq_freq( phys_sf_lun, frequency, s )

      if (mod_tscope.eq.1) then
C     151.5 implies .8Mhz : 151 sets .7
          mod_band_type  = 1
          if ( frequency .eq. 151.5d+6 ) then
                mod_band_width = 0.8D+6/frequency
          else
                mod_band_width = 0.7D+6/frequency
          endif
      else if (mod_tscope.eq.2) then
          mod_band_type  = 2
          mod_band_width = 0.127D+6/frequency
      else
          mod_band_type  = 0
          mod_band_width = 0.0
      end if

C     Initialise model to an infinitely large aperture of unit amplitude
      mod_minirt(u1_ptr) = -8
      mod_minirt(u2_ptr) =  7
      mod_minirt(v1_ptr) =  8
      mod_minirt(v2_ptr) = -7
      mod_minirt(nx_ptr) = 16
      mod_minirt(ny_ptr) = 16
      mod_minirt(dtp_ptr)=  3
      mod_minirt(blk_ptr)= 0
      mod_u_wl2gp   = 0.0
      mod_v_wl2gp   = 0.0
      do 100, i = 1, 16*16
          mod_aper(i) = 1.0
  100 continue

      mod_pb_flg   = .true.
      mod_num_srcs = 0

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_INIT_MODEL' )
          return
      end
