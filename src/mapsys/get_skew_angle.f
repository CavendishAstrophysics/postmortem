C
C+get_skew_angle
C
      SUBROUTINE get_skew_angle(  lsf_num,
     *                            skew_angle,
     *                            s                      )

C
C     Calculates the skew angle for lsf_get_uv using the map redtape.
C
C     Given:
C         Logical sample file number
              integer             lsf_num

C     Returned:
C         Skew angle to be used for lsf_get_uv
              real*8              skew_angle
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Utility routine for map-making that calculates the skew angle
C     for lsf_get_uv so that the map is apparently projected in the
C     correct coordinate system.
C
C     Uses the current map redtape and also updates the phase centre
C     of the lsf to the correct phase centre for this map.
C
C     NPR.    21 September 1987.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/constants.inc'
      include        '/mrao/include/maplib_errors.inc'
      include        '/mrao/include/maplib_redtape.inc'

C     FFTLIB logical function to check integer for power of 2
              external  check2
              logical   check2

C     ****************************************************************
C
C     Local constant and variable declarations
C         Ra and dec, and U and V coordinates corresponding to them.
              real*8          ra, dec, u, v
C         An offset to apply to v corresponding to half a degree
              real*8          offset
C         Logical sample file parameters returned from enquiry routines.
              real*8          lsf_epoch, lsf_ra, lsf_dec
              character*16    src_name
C         Map projection parameters returned from enquiry routines.
              real*8          ref_ra, ref_dec, ref_date, obs_date
              real*8          pr_usamp, pr_skew, pr_epoch, pr_angle
              integer         pr_code
              character*16    source_name

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Check map size is powers of two
c     if ( (nint(2.0**real(nint(alog2(real(ixmax))))) .ne. (ixmax)) .or.
c    *     (nint(2.0**real(nint(alog2(real(iymax))))) .ne. (iymax))    )
c    *                                                      then
      if ( .not.check2(ixmax) .or. .not.check2(iymax)) then
          s = ILL_REDTAPE
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Enquire the map centre and projection.
      call enmapc( ref_ra, ref_dec, ref_date, obs_date, source_name, s )
      call enmapj( pr_code, pr_usamp, pr_skew, pr_epoch, pr_angle, s )

C     Set the projection parameters to equatorial coordinates.
      call stproj( 1, 1, pr_usamp, pr_skew,
     *             ref_ra, ref_dec, ref_date, pr_epoch, s )

C     Find the map centre and set the lsf phase centre to this point.
      u = dble( iumap1 + ixmax/2 )
      v = dble( ivmap1 - iymax/2 )
      call uvtord( u, v, ra, dec, s )
      call lsf_set_pc( lsf_num, pr_epoch, ra, dec, source_name, s )

C     Find out where it's ended up at the observation date.
      call lsf_enq_pc_epoch(  lsf_num,
     *                        lsf_epoch, lsf_ra, lsf_dec, src_name, s )
      if ( s .ne. 0 ) goto 9999

C     Find the ra and dec of the point half a degree away up the v axis
C     from (u,v) and precess it to epoch
      offset = 1800.0D+0*const_sa2r/dble(pr_usamp)
      call uvtord( u, (v+offset), ra, dec, s )
      if ( s .ne. 0 ) goto 9999
      call precrd2( 1, pr_epoch, ra, dec, lsf_epoch, ra, dec )

C     Set projection parameters to observation date defaults.
C     The correct map centre - (u,v), should be at (0,0) and we just
C     have to find where the point (u,v+(0.5 degrees)) has got to.
      call stproj( 1, 0, pr_usamp, 0.0D+0,
     *             lsf_ra, lsf_dec, lsf_epoch, lsf_epoch, s )
      call rdtouv( ra, dec, u, v, s )
      if ( s .ne. 0 ) goto 9999

      skew_angle = datan2( u, v )

C     Reset the projection parameters to map defaults.
      call stproj( pr_code, 1, pr_usamp, pr_skew,
     *             ref_ra, ref_dec, ref_date, pr_epoch, s )
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine GET_SKEW_ANGLE' )
          return
      end
