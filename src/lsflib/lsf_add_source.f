
C     *****************************************************************
C
C+lsf_add_source
C
      SUBROUTINE lsf_add_source(  lsf_num, src_type,
     *                            src_refdat, src_ra, src_dec, src_flux,
     *                            s                                   )

C     Adds a new source to the LSF model visibility definition.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         Source type - 1 if point source, otherwise the model defined
C         by lsf_set_model
              integer             src_type
C         ra, dec and reference date (dec. year) of source phase centre.
              real*8              src_refdat, src_ra, src_dec
C         Flux of source.
              real                src_flux
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C     There are no restrictions on the reference date, it just being
C     the date that the ra and dec are expressed at.
C
C     NPR     6 April 1988.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsf_model.inc'

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      if (sf_type .ne. 1) then
C         Not a physical sample file.
          call io_wrout('sample file of unexpected type.')
C          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Set up runtime common blocks.
      if (mod_num_srcs .lt. mod_max_srcs) then
          mod_num_srcs = mod_num_srcs + 1
          call precrd2(   1,
     *                    src_refdat,
     *                    src_ra, src_dec,
     *                    epoch,
     *                    mod_ra(mod_num_srcs), mod_dec(mod_num_srcs))
          mod_flux(mod_num_srcs) = src_flux
          mod_type(mod_num_srcs) = src_type
      else
          s = ILL_NUMSRCS
      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ADD_SOURCE' )
          return
      end
