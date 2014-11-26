C+ENQ_V2_TELGEOM

       subroutine enq_v2_telgeom ( geom_pars, s )
C
C     Returns the telescope geometry.
C
C     Returned:
C         Array containing telescope geometry parameters
              real*8          geom_pars(10)
C         Status value
              integer         s

C     Support routine for ENQ_TELGEOM
C
C     (DJT, 13/11/95)
*-

       include  '/mrao/post/include/control_tables.inc'
       include  '/mrao/post/include/control_tab_v2.inc'
       include  '/mrao/post/include/samplib_errors.inc'

       if (s.ne.0) return

       geom_pars(1) = elong
       geom_pars(2) = azdatum
       geom_pars(3) = eldatum
       geom_pars(4) = azlong
       geom_pars(5) = Ypole
       geom_pars(6) = latitude
       geom_pars(7) = az_tangent
       geom_pars(8) = el_tangent
       geom_pars(9) = cos_lat
       geom_pars(10) = sin_lat

       end

