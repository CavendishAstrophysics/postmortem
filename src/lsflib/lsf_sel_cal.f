C
C+lsf_sel_cal
C
      SUBROUTINE lsf_sel_cal( lsf_num,
     *                        s                         )

C
C     Asks the user to select the lsf spacing calibration to use.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present simply searches for the data file with the same name
C     as the current sample file. It reads the aerial amplitudes and
C     phases from this, extracts the phases only.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/cal_record.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Number of calibrations on file and the selected one.
              integer         num_cals, new_cal
C         The calibration name
              character*16    source
C         Prompt string
              character*80    prompt
C         Calibration file name
              character*64    cf_name

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

      if (sf_type .ne. 1) then
C         Not a physical sample file.
          call io_wrout('Sample file of unexpected type.')
C          return
      end if

      if ( cf_lun .eq. 0 ) then
C        Calibration file is closed, open it.
         call enq_namfil( sf_lun, 'CAL', cf_name, s )
         if ( s .eq. NO_FILE ) then
           s = 0
           call io_wrout( 'No calibrations exist for this sample file.')
           return
         end if
         call open_sf( cf_lun, cf_name, 'READ', 0, s )
         if ( s .ne. 0 ) goto 9999
      else
C         Close calibrations opened by removes.
          call close_remcal( s )

C         Close calibration currently open.
          call close_source( cf_lun, lsf_cal_num, s )
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      lsf_key     = 1
      lsf_name    = ' '

      call enq_numsrc( cf_lun, num_cals, s )

      i = 1
      new_cal = 0
  100 continue
          call enq_src_name( cf_lun, i, source, s )
          call enq_src_def( cf_lun, i, cal_record, s )

          if (cal_key .ne. 0) then
              write(prompt,'(A,I2,3A)')
     *            'Use calibration number ', i, ' on ', source, ':'

              if ( io_yesno( prompt, 'No', s ) ) new_cal = i
          end if
          i = i + 1
      if ( i.le.num_cals .and. s.eq.0 .and. new_cal.eq.0 ) goto 100

      if (s .eq. USR_BREAK) then
          s = 0
      else if (s.ne.0) then
          goto 9999
      else if (new_cal.eq.0) then
          cal_flag    = 0
          lsf_cal_key = 0
          lsf_cal_num = 0
      else
          cal_flag    = 0
          lsf_cal_key = cal_key
          lsf_cal_num = new_cal
      end if

      if (lsf_cal_num .ne. 0) then
C  [buffer size restricted to 8 pages, DJT 27/11/89]
          call open_source( cf_lun, lsf_cal_num, 8, s )
          call open_remcal( s )
      else
          call open_remcal( s )
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SEL_CAL' )
          return
      end
