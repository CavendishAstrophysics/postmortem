C
C+open_remcal
C
      subroutine open_remcal( s )

C     Opens all the calibrations for the current removes.
C
C     Given:
C         None (updates LSF-RUNTIME common block)

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Internal routine for opening calibrations relating to the current
C     removes. Bulletproof in the sense that if the calibration file is
C     closed, it is opened if necessary and if no calibration sources
C     are open at the end of the routine, then the file is closed.
C     The flag which determines whether the calibration file is
C     currently opened or closed is the unit number cf_lun, in the
C     runtime common block.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/remove_record.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Loop counter
              integer         i, j
C         List of remove calibration keys.
              integer         rem_cal_key(max_rt_rems)
C         Remove ionospheric correction key.
              integer         ion_key
C         Calibration file file name
              character*64    file_name
C         Flag set if a calibration source is opened
              logical         source_opened

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
      source_opened = (lsf_cal_key.ne.0)
      do 200, i = 1, num_removes
          call enq_src_def( rf_lun, lsf_rem_num(i), remove_record, s )
          lsf_rem_type(i) = rem_type

C         Find how the remove is calibrated.
          call lsf_enq_cal( sf_lun, rem_lsf,
     *                      ion_key, rem_cal_key(i), s )

C         Now open the calibration source
          if (rem_cal_key(i).ne.0) then
              if (cf_lun.eq.0) then
                  call enq_namfil( sf_lun, 'CAL', file_name, s)
                  call open_sf( cf_lun, file_name, 'READ', 0, s )
              end if

C             Find if the source is opened
              lsf_rem_cal(i) = 0
              if (rem_cal_key(i).eq.lsf_cal_key)
     *            lsf_rem_cal(i)=lsf_cal_num

              do 100, j = 1, i-1
                  if (rem_cal_key(i).eq.rem_cal_key(j))
     *                                 lsf_rem_cal(i)=lsf_rem_cal(j)
  100         continue

              if (lsf_rem_cal(i).eq.0) then
C  [Buffer size reduced to 8 pages, DJT, 29/11/89]
                  call open_srckey( cf_lun, rem_cal_key(i), 8,
     *                              lsf_rem_cal(i), s         )
                  source_opened = .true.
              end if
          else
              lsf_rem_cal(i) = 0
          end if

          if (ion_key.ne.0) then
              call enq_ion_num( sf_lun, ion_key,
     *                          lsf_rem_ion(i), s    )
          else
              lsf_rem_ion(i) = 0
          end if
  200 continue

      if (.not.source_opened) then
          call close_sf( cf_lun, s )
          cf_lun = 0
      end if

      if (s.ne.0) return
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine OPEN_REMCAL' )
          return
      end
