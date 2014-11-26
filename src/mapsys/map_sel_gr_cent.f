
C     *****************************************************************
C
C+map_sel_gr_cent
C
      SUBROUTINE map_sel_gr_cent ( s )

C     Asks the user to select the RA, dec and epoch for the map grid.
C
C     Given:
C         None.
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Asks user for RA, dec and Epoch of the (0,0) point of the map
C     grid.
C
C     NPR.    22 September 1987.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include    '/mrao/include/iolib_errors.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Ra, dec and reference date of grid centre
              real*8              ra, dec, ref_date, obs_date
C         Name of grid centre position
              character*(20)      source_name
C         Projection parameters (needed for map epoch)
              integer             pr_code
              real*8              pr_samp, pr_skew, pr_epoch, pr_angle

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
      call enmapc( ra, dec, ref_date, obs_date, source_name, s )
      call enmapj( pr_code, pr_samp, pr_skew, pr_epoch, pr_angle, s )

      call get_source( 'Source at grid centre : ', ' ', pr_epoch,
     *                 ref_date, ra, dec, source_name, s      )

      call stmapc( ra, dec, ref_date, obs_date, source_name, s )

      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. usr_break ) then
              call map_wrerr( s, 'in subroutine MAP_SEL_GR_CENT' )
          end if
          return
      end
