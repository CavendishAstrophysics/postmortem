
C     *****************************************************************
C+lsf_set_pc
C
      SUBROUTINE lsf_set_pc(  lsf_num,
     *                        pc_refdat, pc_ra, pc_dec, pc_text,
     *                        s                                   )

C     Sets the lsf phase centre to a given RA and dec.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C         ra, dec and reference date (decimal year) of new phase centre.
              real*8              pc_refdat, pc_ra, pc_dec
C         Description of new phase centre.
              character*(*)       pc_text
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C     A non-interactive routine for setting the lsf phase centre. If the
C     new phase centre is the same as the old one then the LSF
C     definition is not changed but the values in the runtime common
C     blocks are re-initialised. There are no restrictions on the
C     reference date, it just being the date that the ra and dec are
C     expressed at.
C
C     NPR     30 November 1987.
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Planet index from planet-lib
              integer         pl_index

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
C     Set up logical sample file definition
      if ( pc_ra     .eq. ref_ra   .and.
     *     pc_dec    .eq. ref_dec  .and.
     *     pc_refdat .eq. ref_date .and.
     *     (pc_text.eq.source_text .or. pc_text .eq. ' ') ) then
C         Null routine
          continue
      else
          lsf_key     = 1
          lsf_name    = ' '
          call util_enqnow( lsf_time_created )
          lsf_last_used = lsf_time_created
          ref_ra      = pc_ra
          ref_dec     = pc_dec
          ref_date    = pc_refdat
          source_text = pc_text
          source_ucase= pc_text
          call chr_chucas( source_ucase )
          call planet_index( source_ucase, pl_index, s )
          planet_flg = ( s .eq. 0 )
          s = 0
      end if

C     Set up runtime common blocks.
      call precrd2(1, ref_date,ref_ra,ref_dec, epoch,epoch_ra,epoch_dec)

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_PC' )
          return
      end
