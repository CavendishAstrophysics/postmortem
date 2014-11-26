


C+ENQ_PATH_COMP

       subroutine enq_path_comp ( lun, pc_ra, pc_dec, s )
C
C     Returns the path compensation centre of a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer         lun
C
C     Returned:
C         Phase center of path compensators at epoch (I think).
              real*8          pc_ra, pc_dec
C         Status
              integer         s
C
C     NPR     14 July 1987,  PA    1 March 1989
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_path_comp ( pc_ra, pc_dec, s )
      else if (ct_vers .eq. 2) then
         call enq_v2_path_comp ( pc_ra, pc_dec, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_PATH_COMP' )

      end
