

C+ENQ_xyz

       subroutine enq_xyz (lun, xyz_coord, s )
C
C     Returns the xyz offsets (in 0.1 mm) from the nominal positions
C       for mobile aerials, the values are the sums of the
C       aerial and obs position values;
C       see /mrao/post/include/ryle_tel_pars.inc for details
C
C     Given:
C         Sample file Fortran logical unit number
              integer         lun
C
C     Returned: 3*8 array x(1), y(1), z(1), x(2), ...

              integer     xyz_coord(3, 8)

C         and status value
              integer     s

C     GP  2 March 1999
C
*-
        integer i, j

        include '/mrao/post/include/control_tables.inc'
        include '/mrao/post/include/control_tab_v2.inc' 
        include '/mrao/post/include/samplib_errors.inc'

      if (s .ne. 0) return

      call read_ct(lun, s)
      if (s .ne. 0) goto 999

      if (ct_vers .eq. 2) then
        do i = 1, 3
            do j = 1,8
                xyz_coord(i,j) = xyz(i,j)
            enddo
        enddo
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr(s, 'in subroutine ENQ_xyz')

      end
