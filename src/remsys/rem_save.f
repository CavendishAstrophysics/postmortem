C
C+rem_save
C
      SUBROUTINE rem_save ( psf_name, s )

C
C     Saves the current remove runtime common blocks as a remove.
C
C     Given:
C         The name of the physical sample file - must be closed.
              character*(*)       psf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include '/mrao/include/iolib_functions.inc'
C
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/remove_common.inc'
      include '/mrao/post/include/src_pack.inc'

C     ****************************************************************
C
C     Local variables
C         Dummy variable
              integer             i
C         Logical unit number
              integer             lun
C         Logical sample file number
              integer             lsf_num
C         Number of buffers and number of visibilities in the lsf
              integer             num_buff, num_vis
C         First and last sample in the logical sample file.
              integer             first_samp, last_samp
C         Maximum amplitude in the remove file
C             real                max_rem_amp
C         Remove file name
              character*64        rem_file_name

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     enquire what remove file name should be
C         suppress no such file

c     call chr_chucas( psf_name )
      call open_sf( lun, psf_name, 'READ', 0, s )
      call enq_namfil( lun, 'REM', rem_file_name, s )
      if ( s.eq.NO_FILE ) s = 0
      call close_sf( lun ,s )

C     see if there would be a name clash
C         a possibility if a remove file is being used as the sample fie
      if ( psf_name .eq. rem_file_name ) then
        call io_wrout('Sample file name cannot be the Remove file name')
        return
      endif
c     if ( psf_name(1:5) .ne. '(SAMP' ) then
c       call io_wrout( 'Sample file must be in a (samp-:) directory.' )
c       return
      if ( rem_lsf .eq. 1 ) then
        call io_wrout( 'Remove logical sample file must be saved.' )
        call lsf_open( psf_name, rem_lsf, 'READ', lsf_num, s )
        call lsf_save( lsf_num, rem_lsf, s )
        call lsf_close( lsf_num, 0 )
        if ( s.ne.0 ) goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( rem_number .ne. 0 ) then
          if ( io_yesno( 'Create a new remove ?', 'Yes', s ) ) then
              rem_number = 0
          end if
          if ( s .ne. 0 ) goto 9999
          rem_key = 0
      end if

      if ( rem_number .eq. 0 ) then
          call lsf_open( psf_name, rem_lsf, 'READ', lsf_num, s )
          call lsf_enq_numbuff( lsf_num, num_buff, s )
          call lsf_enq_samples( lsf_num, 1, first_samp, last_samp, s )
          call lsf_enq_samples( lsf_num, num_buff, i, last_samp, s )
          call lsf_close( lsf_num, s )
C         Open the physical sample file and create the source.
          call open_sf( lun, psf_name, 'READ', 0, s )
          call open_source( lun, 1, 0, s )
          call enq_numvis( lun, num_vis, s )
          call create_source( lun, 'REM',
     *                        first_samp, last_samp,
     *                        num_buff, num_vis,
     *                        rem_number, s                      )
C         Ascertain the remove file name.  already done
C          call enq_namfil( lun, 'REM', rem_file_name, s )
          call close_sf( lun, s )
C      else
C         Ascertain the remove file name               already done
C          call open_sf( lun, psf_name, 'READ', 0, s )
C          call enq_namfil( lun, 'REM', rem_file_name, s )
C          call close_sf( lun, s )
      end if

      call open_sf( lun, rem_file_name, 'WRITE', 0, s )
C     old code to set scaling of remove file (NR funny removes)
C      if (rem_type .eq. 1) then
C          max_rem_amp = 100
C      else if (rem_type .eq. 2 ) then
C          max_rem_amp = rem_src_flux
C      end if

C      if (rem_type.eq.1 .or. rem_type .eq. 2) then
C         Amplitude is scaled to unity so reset amplitude factor.
C          call enq_src_pack( lun, rem_number, src_pack, s )
C          src_amp_factor = 32767.0/max_rem_amp
C          call set_src_pack( lun, rem_number, src_pack, s )
C      end if

C     New version to scale to user supplied max. amp
      if (rem_type .eq. 2 .and. rem_src_type.eq.2 ) then
          call enq_src_pack( lun, rem_number, src_pack, s )
          src_amp_factor = 32767.0/(rem_model_max*rem_src_flux(1))
          call set_src_pack( lun, rem_number, src_pack, s )
      end if

      call set_src_def( lun, rem_number, remove_record, s )
      call close_sf( lun, s )
      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call rem_wrerr( s, 'in subroutine REM_SAVE' )
          end if
          return
      end
