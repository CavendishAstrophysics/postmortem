C
C+lsf_sel_ph_cent
C
      SUBROUTINE lsf_sel_ph_cent( lsf_num,
     *                            s                      )

C
C     Asks the user to select the lsf phase centre.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present simply asks for a new phase centre.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_errors.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Source name, ra, dec, and reference date
              character*(20)      source_name
              real*8              ra, dec, ref_date
C         Logical sample file epoch
              real*8              epoch
C         Sample file information
              integer             sf_lun, src_num

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

      source_name = ' '

      call lsf_enq_pc_epoch( lsf_num,
     *                       epoch, ra, dec, source_name, s,   )

      call get_source( 'Source-name : ', ' ', epoch,
     *                  ref_date, ra, dec, source_name, s      )

      if (ra.lt.0.0 .and. dec.lt.0.0 .and. s.eq.0) then
C .. reset ra/dec to phase centre at reference date
        call lsf_enq_sf( lsf_num, sf_lun, src_num, s )
        call enq_pc_epoch( sf_lun, src_num,
     *                     ref_date, ra, dec, source_name, s )
      end if

      call lsf_set_pc(  lsf_num,
     *                  ref_date, ra, dec, source_name, s      )
      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call lsf_wrerr( s, 'in subroutine LSF_SEL_PH_CENT' )
          end if
          return
      end
