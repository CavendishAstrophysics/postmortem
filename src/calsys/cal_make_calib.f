C
C+cal_make_calib
C
      SUBROUTINE cal_make_calib( file_name, s )

C     Calculates all outstanding calibrations for a calibration file.
C
C     Given:
C         calibration file name.
              character*(*)       file_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'

C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/calib_errors.inc'
      include  '/mrao/post/include/cal_common.inc'

C
C     Variables, equivalences and commons
C         Current output device
              integer         out
C         Calibration file unit number
              integer         cf_lun
C         Number of sources in the calibration file and current source num.
              integer         num_srcs, src_num
C         Flag set if calibration made correctly
              logical         cal_made

C     ==================================================================
C
C     Subroutine initialisation
C     -------------------------
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      call io_enqout( out )

      call open_sf( cf_lun, file_name, 'WRITE', 0, s )
      call enq_numsrc( cf_lun, num_srcs, s )

C
C     Main Code
C     ---------
C
      cal_made = .false.
      do 1000, src_num = 1, num_srcs
          call enq_src_def( cf_lun, src_num, cal_record, s )
          cal_number = src_num

          if ( (cal_key .eq. 0) .and.
     *         (cal_type .eq. 1 .or. cal_type .eq. 2 .or.
     *          cal_type .eq. 3                        ) ) then
              write(out,*) 'Making calibration for ', cal_source
              call cal_make_type( cf_lun, src_num, s )
              if (s.eq.0) cal_made = .true.
          else if ( cal_key .eq. 0 ) then
C             Calibration not made, but how to make it is undefined
              s = ILL_CALIBRATION
              goto 9999
          end if
 1000 continue

      call close_sf( cf_lun, s )
c     if (cal_made) call save_sf( file_name, s )
      if ( s .ne. 0 ) goto 9999

      return

C
C     Error Handling
C     --------------
C
 9999 continue
          call cal_wrerr( s, 'in subroutine cal_make_calib ' )
          return
      end
