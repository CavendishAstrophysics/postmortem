

C+ENQ_GEOMETRY

       subroutine enq_geometry ( lun, iae, xyz_coord, s )
C
C     Returns the geometry for the specified aerial.
C
C     Given:
C         Sample file Fortran logical unit number
              integer         lun
C         Aerial number
              integer         iae
C
C     Returned:
C         Coordinates of the aerial, expressed in seconds of time
              real*8          xyz_coord(3)
C         Status value
              integer     s

C     DJT, 12/8/91
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_geometry (iae, xyz_coord, s)
      elseif (ct_vers .eq. 2) then
         call enq_v2_geometry (iae, xyz_coord, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_GEOMETRY' )

      end
