C
C+cal_save
C
      SUBROUTINE cal_save ( psf_name, s )

C
C     Saves the current calibration definition.
C
C     Given:
C         The name of the physical sample file - must be closed.
              character*(*)       psf_name

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C last mod 12 April 2000 GP   [io_setacc]
C-
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'
C
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'

C
C     Local variables
C         Logical unit number
              integer             lun
C         File name of the sample file used for the calibration.
              character*64        cal_sf_name
C         Logical sample file number
              integer             lsf_num
C         Number of buffers and number of visibilities in the lsf
              integer             num_buff, num_vis
C         Number of samples in the physical sample file
              integer             num_samp
C         Calibration file name
              character*64        cal_file_name


C     ==================================================================

C     Subroutine initialisation
C     -------------------------

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call open_sf( lun, psf_name, 'READ', 0, s )
      call enq_namfil( lun, 'CAL', cal_file_name, s )
      if ( s.eq.NO_FILE ) s = 0
      call close_sf( lun ,s )


      if ( psf_name .eq. cal_file_name  ) then
          call io_wrout(
     *          'Sample file name cannot be the same as Cal file')
          return
      endif
      cal_sf_name = cal_sf
      if ( cal_lsf .eq. 1 ) then
          call io_wrout(
     *         '*** Correction logical sample file must be saved' )
          call lsf_open( cal_sf_name, cal_lsf, 'READ', lsf_num, s )
          call lsf_save( lsf_num, cal_lsf, s )
          call lsf_close( lsf_num, 0 )
          if ( s.ne.0 ) goto 9999
      end if

C
C     Main Code
C     ---------
C
      if ( cal_number .ne. 0 ) then
          if ( io_yesno(
     *              'Create a new calibration ? ', 'Yes', s ) ) then
              cal_number = 0
          end if
          if ( s .ne. 0 ) goto 9999
          cal_key = 0
      end if

      if ( cal_number .eq. 0 ) then
          call lsf_open( cal_sf_name, cal_lsf, 'READ', lsf_num, s )
          call lsf_enq_numbuff( lsf_num, num_buff, s )
          call lsf_close( lsf_num, s )

          call open_sf( lun, psf_name, 'READ', 0, s )
          call open_source( lun, 1, 0, s )
          call enq_numsamp( lun, 1, num_samp, s )
          call enq_numvis( lun, num_vis, s )
          call create_source( lun, 'CAL',
     *                        1, num_samp,
     *                        num_buff, num_vis,
     *                        cal_number, s                      )
          call close_source( lun, 1, s )
          call close_sf( lun, s )

      end if

      call open_sf (lun, cal_file_name, 'WRITE', 0, s)
      call io_setacc (cal_file_name, 'r', 'rw', 'rw', s)
      call set_src_def (lun, cal_number, cal_record, s)
      call close_sf (lun, s)
      if ( s .ne. 0 ) goto 9999

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call cal_wrerr( s, 'in subroutine cal_save ' )
          end if
          return
      end
