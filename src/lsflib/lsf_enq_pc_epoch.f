
C     *****************************************************************
C
C+lsf_enq_pc_epoch
C
      SUBROUTINE lsf_enq_pc_epoch(    lsf_num,
     *                                lsf_epoch, ra, dec, description,
     *                                s                    )

C
C     Returns sample file epoch phase centre information of a lsf.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Phase centre and epoch of the lsf.
              real*8              lsf_epoch, ra, dec
C         String description of phase centre (i.e. source name)
              character*(*)       description
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     The routine returns the logical sample file phase centre expressed
C     at the epoch of the lsf. The significance of this is that the
C     ra and dec returned are the actual ra and dec that the lsf phase
C     rotation is done to.
C
C     NPR     2 December 1987.
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
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
      lsf_epoch   = epoch
      call precrd2( 1, ref_date,ref_ra,ref_dec, epoch, ra, dec )
      description = source_text

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_PC_EPOCH' )
          return
      end
