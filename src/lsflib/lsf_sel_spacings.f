C
C     ******************************************************************
C
C+lsf_sel_spacings
C
      SUBROUTINE lsf_sel_spacings( lsf_num,
     *                            s                      )

C
C     Asks the user to select the logical sample file spacings.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function declarations and global constants
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/global_constants.inc'
C

C     ****************************************************************
C
C     Local variables, equivilances and commons
C         Sample file logical unit number and source number
              integer             sf_lun, src_num
C         Spacing list length and list
              integer             num_spac, spacings( max_vis )
C         Spacing list text description
              character*80        description

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C

      call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
      call get_spacings( sf_lun, 'Spacing list : ', 'All', description,
     *                    spacings, max_vis, num_spac, s )
      call lsf_set_spacings( lsf_num, num_spac, spacings, 1, s )
      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine LSF_SEL_SPACINGS' )
          end if
          return
      end
