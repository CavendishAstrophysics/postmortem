

C+ENQ_CHGEOM

       subroutine enq_chgeom ( lun, iae, iba, ich, xyz_coord, s )
C
C     Returns the geometry for the specified aerial and channel
C
C     Given:
C         Sample file Fortran logical unit number.
              integer         lun
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
              integer     s

C     PA, 28/2/89
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_chgeom (iae, iba, ich, xyz_coord, s)
      elseif (ct_vers .eq. 2) then
         call enq_v2_chgeom (iae, iba, ich, xyz_coord, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_CHGEOM' )

      end
