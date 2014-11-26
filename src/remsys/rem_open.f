C
C+rem_open
C
      subroutine rem_open( psf_name, lsf_key, s )

C     Asks the user to select an existing remove to open.
C
C     Given:
C         Physical sample file name.
              character*(*)       psf_name

C     Returned:
C         Key of LSF used in remove
              integer             lsf_key
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/remove_common.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Remove file name
              character*64    rem_file_name
C         Number of removes in remove file
              integer         num_removes
C         General purpose file unit number.
              integer         lun
C         Remove name
              character*20    source
C         Flag set if current remove is the one wanted.
              logical         found

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call open_sf( lun, psf_name, 'READ', 0, s )
      call enq_namfil( lun, 'REM', rem_file_name, s )
      if (s .eq. NO_FILE) then
        s = 0
        call io_wrout( 'No removes have been saved for this file yet.')
        call close_sf( lun, s )
        return
      else
        call close_sf( lun, s )
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call open_sf( lun, rem_file_name, 'READ', 0, s )
      call enq_numsrc( lun, num_removes, s )

      i = 1
      found = .false.
  100 continue
          call enq_src_name( lun, i, source, s )
          if ( io_yesno( 'Open remove on '//source//' ?','No' ,s)) then
              found = .true.
              call enq_src_def( lun, i, remove_record, s )
              rem_number = i
          else
              i = i + 1
          end if
      if ( i.le.num_removes .and. s.eq.0 .and. .not.found ) goto 100

C     Restore status for user break.
      if ( s .eq. usr_break ) s = 0
      call close_sf( lun, s )
      lsf_key = rem_lsf

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call rem_wrerr( s, 'in subroutine REM_OPEN' )
          return
      end
