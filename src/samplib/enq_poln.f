

C+ENQ_POLN

       subroutine enq_poln ( lun, poln, s )
C
C     Returns the polarization code for the sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         Polarization code
               integer     poln
C         Status
              integer     s
C
C     PA      12 January 1989
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_poln( poln, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_poln( poln, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_POLN' )

      end
