

C+ENQ_V1_CHGEOM

       subroutine enq_v1_chgeom ( iae, iba, ich, xyz_coord, s )
C
C     Returns the geometry for the specified aerial and channel
C
C     Given:
C         aerial number
              integer         iae
C         sub-band number
              integer         iba
C         channel number
              integer         ich
C
C     Returned:
C         Coordinates of the telescope/band/channel, in wavelengths
              real*8          xyz_coord(3)
C         Status
              integer         s

C     PA, 28/2/89
C
*-

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

       if (s.ne.0) return

       if (iae.ge.1 .and. iae.le.max_aes) then
         xyz_coord(1) = x(iae)
         xyz_coord(2) = y(iae)
         xyz_coord(3) = z(iae)
       else
         s = ILL_AERIAL
         goto 999
       end if

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_V1_CHGEOM' )

       end
