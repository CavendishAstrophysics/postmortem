C
C+ion_save
C
      SUBROUTINE ion_save( sf_name, s )

C
C     Saves the current ionospheric correction definition.
C
C     Given:
C         The name of the physical sample file - must be closed.
              character*(*)       sf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function Declarations
C
      include        '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables
C         Sample file unit number and LSF number.
              integer             sf_lun, lsf_num

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call chr_chucas( sf_name )
c     if ( sf_name(1:5) .ne. '(SAMP' ) then
c       call io_wrout( 'Sample file must be in a (samp-:) directory.' )
c       return
      if ( ion_lsf .eq. 1 ) then
        call io_wrout( 'Correction logical sample file must be saved.' )
        call lsf_open( sf_name, ion_lsf, 'READ', lsf_num, s )
        call lsf_save( lsf_num, ion_lsf, s )
        call lsf_close( lsf_num, 0 )
        if ( s.ne.0 ) goto 9999
      end if


C     ****************************************************************
C
C         Main Code
C         ---------
C
      call open_sf( sf_lun, sf_name, 'WRITE', 0, s )
      call open_source( sf_lun, 1, s )
      if (s .ne. 0) goto 9999

      if (ion_number .ne. 0 .and. ion_type .ne. 0) then
          if (io_yesno( 'Create new correction ?', 'Yes', s )) then
              ion_number = 0
          end if
      end if

      if (ion_number .eq. 0) then
          call create_ion_corr(   sf_lun,
     *                            ion_first, ion_last, ion_number,
     *                            s                                 )
      end if

      ion_key = 0
      call set_ion_corr(  sf_lun, ion_number,
     *                    ion_type, ion_key, ion_lsf,
     *                    ion_numsrcs, ion_source, ion_ra, ion_dec,
     *                    ion_param, s )

C     Restore status if user break
      if (s .eq. USR_BREAK) s = 0
      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )
      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call ion_wrerr( s, ' in subroutine ion_save ' )
          end if
          return
      end
