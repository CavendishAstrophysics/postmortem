C
C+calibrate
C
      SUBROUTINE calibrate(   lun,
     *                        cal_num,
     *                        num_vis,
     *                        vis,
     *                        sp_list,
     *                        vis_sid,
     *                        apply_flag,
     *                        s               )

C
C     Applies the given calibration to the given visibility buffer.
C
C     Given:
C         Calibration file unit number.
              integer         lun
C         Source number of the calibration in the calibration file.
              integer         cal_num
C         Number of spacings.
              integer*4       num_vis
C         Visibility buffer.
              complex         vis( num_vis )
C         Spacing list
              integer         sp_list( num_vis )
C         The sidereal time of the visibility buffer
              integer         vis_sid
C         Flag set if calibration is to be applied, unset to remove
              logical         apply_flag

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Applies spacing calibrations to the visibility buffer.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/global_constants.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer         i
C         Calibration file redtape.
              integer         samp_num
C         Calibration data
              complex         cal_list( max_vis )

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      if (cal_num .eq. 0) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call read_sample_sid(   lun, cal_num, vis_sid, samp_num,
     *                        num_vis, sp_list, cal_list, s )
      if (s .ne. 0) goto 9999

      if (apply_flag) then
          do 100, i = 1, num_vis
              vis( i ) = vis( i ) * cal_list( i )
  100     continue
      else
          do 200, i = 1, num_vis
              vis( i ) = vis( i ) / cal_list( i )
  200     continue
      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine CALIBRATE' )
          return
      end
