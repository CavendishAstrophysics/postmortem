
C     *****************************************************************
C
C$(5) Currently open logical sample file enquiry routines.
C
C+lsf_enq_cf
C
      SUBROUTINE lsf_enq_cf( lsf_num,
     *                       cal_sf_lun,
     *                       cal_src_num,
     *                       s                    )

C
C     Returns the calibration unit number and source of a lsf.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C
C     Returned:
C         Fortran unit number of calibration sample file.
              integer             cal_sf_lun
C         Calbiration source number of logical sample file.
              integer             cal_src_num
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This routine is primarily so that programs outside the LSFLIB
C     routines can access the calibration file control tables
C     via the SAMPLIB enquiry routines and/or write new data
C     to the calibration file.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons

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

C     ****************************************************************
C
C         Main Code
C         ---------
C
      cal_sf_lun = cf_lun
      cal_src_num = lsf_cal_num

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_ENQ_CF' )
          return
      end
