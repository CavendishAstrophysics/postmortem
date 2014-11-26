C
C$(6) Logical sample file enquiry routines for any LSF (enquire by key)
C
C+lsf_enq_cal
C
      subroutine lsf_enq_cal ( psf_lun, key, ion_key, calib_key, s )

C     Returns the calibration definition for an unopened LSF file.
C
C     Given:
C         Physical sample file unit number (PSF must be open).
              integer         psf_lun
C         Logical sample file key
              integer         key

C     Returned:
C         Calibration and ionospheric correction keys of LSF.
              integer         ion_key, calib_key
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Possible return status's:
C     NO_LSFSAVED - opening with a key of zero, but no LSF's are saved.
C     ILL_LSF     - Error in the lsf definition or trying to do an
C                   interactive open non-interactively.
C     Other       - Unexpected system error.
C
C     NPR     7 October 1988
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/cal_record.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Loop counter
              integer         i
C         Logical sample file save buffer
              integer         lsf_buffer(lsf_len)
C         Flags set if LSF patched and if LSF not current LSF
              logical         patched, lsf_dumped
C         Calibration file unit number
              integer         lun
C         Calibration file file name
              character*64    file_name

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
      lsf_dumped = .false.
      if (key.eq.0 .and. lsf_key.ne.0) then
          s = ILL_LSF
      else if (key.ne.lsf_key) then
C         Not current LSF - save current definiton
          lsf_dumped = .true.
          do 100, i = 1, lsf_len
              lsf_buffer(i) = log_samp_file(i)
  100     continue

          call lsf_read_def( psf_lun, key, log_samp_file, s )
      end if

      patched = .false.
      if (s.eq.0) then
          if (lsf_ion_key.eq.0.and.ion_flag.ne.0) then
C             File needs patching
              lsf_ion_key = ion_flag
              patched = .true.
          end if

          if (lsf_cal_key.eq.0.and.cal_flag.ne.0) then
C             LSF needs patching, open calibration file.
              if (psf_lun.ne.sf_lun .or. cf_lun.eq.0) then
                  call enq_namfil( psf_lun, 'CAL', file_name, s )
                  call open_sf( lun, file_name, 'READ', 0, s )
              else
                  lun = cf_lun
              end if

              call enq_src_def( lun, cal_flag, cal_record, s )
              lsf_cal_key = cal_key

C             Close file
              if (psf_lun.ne.sf_lun .or. cf_lun.eq.0)
     *            call close_sf(lun, s )
              patched = .true.
          end if

          if (patched) call lsf_write_def(psf_lun, key, log_samp_file,s)
      end if

      if (s.eq.0) then
          calib_key = lsf_cal_key
          ion_key   = lsf_ion_key
      end if

      if (lsf_dumped) then
C         Restore LSF.
          do 200, i = 1, lsf_len
              log_samp_file(i) = lsf_buffer(i)
  200     continue
      end if

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_CAL' )
          return
      end
