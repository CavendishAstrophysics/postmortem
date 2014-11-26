

C+close_remcal
C
      subroutine close_remcal( s )

C     Closes all the calibrations for the current removes.
C
C     Given:
C         None (updates LSF-RUNTIME common block)

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Internal routine for closing calibrations relating to the current
C     removes. The calibration file is not closed.
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

C     ****************************************************************
C
C     Local constant and variable declarations
C         Loop counters
              integer         i, j
C         Calibration file source number
              integer         cf_source

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
      do 200, i = 1, num_removes
          cf_source = lsf_rem_cal(i)
          if ( cf_source.ne.0 .and. cf_source.ne.lsf_cal_num ) then
              call close_source( cf_lun, cf_source, s )

              do 100, j = i, num_removes
                  if (lsf_rem_cal(j).eq.cf_source) lsf_rem_cal(j)=0
  100         continue
          end if

          lsf_rem_type(i) = 0
          lsf_rem_ion(i)  = 0
  200 continue

      if (s.ne.0) return
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine CLOSE_REMCAL' )
          return
      end
