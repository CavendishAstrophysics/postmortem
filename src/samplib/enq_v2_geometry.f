

C+ENQ_V2_GEOMETRY

       subroutine enq_v2_geometry ( iae, xyz_coord, s )
C
C     Returns the geometry for the specified aerial.
C
C     Given:
C         Aerial number
              integer         iae
C
C     Returned:
C         Coordinates of the aerial, in seconds of time
              real*8          xyz_coord(3)
C         Status value
              integer         s

C     DJT, 12/8/91
*-

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

       if (s.ne.0) return


       if (iae.ge.1 .and. iae.le.max_aes) then
         xyz_coord(1) = x8(iae)*1.0d-9
         xyz_coord(2) = y8(iae)*1.0d-9
         xyz_coord(3) = z8(iae)*1.0d-9
       else
         xyz_coord(1) = 0.0d0
         xyz_coord(2) = 0.0d0
         xyz_coord(3) = 0.0d0
         s = ILL_AERIAL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V2_GEOMETRY' )

       end
