

C+ENQ_NUMSP

       subroutine enq_numsp ( lun, numsp, s )
C
C     Returns the number of spacings in a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         Number of spacings
              integer     numsp
C         Status
              integer     s
C
C     NPR     14 July 1987
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_numsp( numsp, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_numsp( numsp, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_NUMSP' )

      end
