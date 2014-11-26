

C+ENQ_GROUPS

       subroutine enq_groups ( lun, ngroups, s )
C
C     Returns the number of aerial groups
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         Number of aerial groups
               integer    ngroups
C         Status
              integer     s
C
C     NPR     14 July 1987
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         ngroups = max_huts
      elseif (ct_vers .eq. 2) then
         ngroups = 1
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_GROUPS' )

      end
