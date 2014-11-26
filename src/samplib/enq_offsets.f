

C+ENQ_OFFSETS

       subroutine enq_offsets ( lun, iae, hoff, doff, s )
C
C     Returns the HA and DEC fixed encoder offsets
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C         Aerial number
              integer     iae
C
C     Returned:
C         HA and DEC encoder offsets
               real       hoff, doff
C         Status
              integer     s
C
C     PA 12/11/88
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_offsets( iae, hoff, doff, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_offsets( iae, hoff, doff, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_OFFSETS' )

      end
