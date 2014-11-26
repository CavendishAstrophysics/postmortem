C+ENQ_TELGEOM

       subroutine enq_telgeom ( lun, geom_pars, s )
C
C     Returns the telescope geometry (Ryle Telescope only).
C
C     Given:
C         Sample file Fortran logical unit number
              integer         lun
C
C     Returned:
C         Array containing telescope geometry parameters
              real*8          geom_pars(10)
C         Status value
              integer         s
C
C     Returned parameters are:
C       longitude (east), az and elev of datum line, pole Y, latitude,
C       azimuth and elevation of the datum line in the tangent plane,
C       cos, sin latitude.   All angles in radians.
C             geom_pars(1)     elong
C             geom_pars(2)     azdatum
C             geom_pars(3)     eldatum
C             geom_pars(4)     azlong
C             geom_pars(5)     ypole
C             geom_pars(6)     latitude
C             geom_pars(7)     az_tangent
C             geom_pars(8)     el_tangent
C             geom_pars(9)     cos_lat
C             geom_pars(10)    sin_lat
C
C     (DJT, 13/11/95)
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call io_wrout('*** not available for CLFST')
      elseif (ct_vers .eq. 2) then
         call enq_v2_telgeom (geom_pars, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_TELGEOM' )

      end



