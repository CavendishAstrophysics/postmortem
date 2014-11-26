C
C+cal_open
C
      subroutine cal_open( psf_name, lsf_key, s )

C     Asks the user to select an existing calibration to open.
C
C     Given:
C         Physical sample file name
              character*(*)       psf_name

C     Returned:
C         Key of LSF used in calibration (unchanged if not same sf.)
              integer             lsf_key
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C
C     Function declarations
C
      include        '/mrao/include/chrlib_functions.inc'
      include        '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/cal_common.inc'

C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Calibration file name
              character*64    cal_file_name
C         Number of removes in calibration file
              integer         num_cals
C         General purpose file unit number.
              integer         lun
C         Calibration name
              character*20    source
C         Flag set if current calibration is the one wanted.
              logical         found

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call open_sf( lun, psf_name, 'READ', 0, s )
      call enq_namfil( lun, 'CAL', cal_file_name, s )
      if (s .eq. NO_FILE) then
          s = 0
          call io_wrout(
     *             'No calibrations have been saved for this file.')
          call close_sf( lun, s )
          return
      else
          call close_sf( lun, s )
      end if

C
C     Main Code
C     ---------
C
      call open_sf( lun, cal_file_name, 'READ', 0, s )
      call enq_numsrc( lun, num_cals, s )

      i = 1
      found = .false.
  100 continue
          call enq_src_name( lun, i, source, s )
          if ( io_yesno( 'Open calibration on '//source//' ? ',
     *                'No', s                                   )) then
              found = .true.
              call enq_src_def( lun, i, cal_record, s )
              cal_number = i
          else
              i = i + 1
          end if
      if ( i.le.num_cals .and. s.eq.0 .and. .not.found ) goto 100

C     Restore status for user break.
      if ( s .eq. usr_break ) s = 0
      call close_sf( lun, s )
      if (chr_fmatch( cal_sf, psf_name )) lsf_key = cal_lsf

      if ( s .ne. 0 ) goto 9999
      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr( s, 'in subroutine cal_open')
          return
      end
