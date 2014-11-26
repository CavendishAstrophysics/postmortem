
C     *****************************************************************
C
C+lsf_enq_pc_rdate
C
      SUBROUTINE lsf_enq_pc_rdate(    lsf_num,
     *                                refdat, ra, dec, description,
     *                                s                    )

C
C     Returns reference date phase centre information of a lsf.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Phase centre and reference date of the lsf.
              real*8              refdat, ra, dec
C         String description of phase centre (i.e. source name)
              character*(*)       description
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The routine returns the logical sample file phase centre expressed
C     at a reference date compatible with the maplib definition of
C     dates and positions.
C
C     This means that the positions and date are 1950.0 except if the
C     declination is the +/- 90 (or const_pyby2 in radians), when the
C     reference date is at the date the position needs to be expressed
C     at.
C
C     NPR     2 December 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

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

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if (ref_dec .eq. const_piby2 .or. ref_dec .eq. -const_piby2) then
          refdat = ref_date
          ra     = ref_ra
          dec    = ref_dec
          description = source_text
      else
          refdat = 1950.0D+0
          call precrd2( 1, ref_date, ref_ra,ref_dec, refdat, ra, dec )
          description = source_text
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_PC_RDATE' )
          return
      end
