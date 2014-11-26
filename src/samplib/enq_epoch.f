

C+ENQ_EPOCH

       subroutine enq_epoch ( lun, epoch, s )
C
C     Returns the epoch of a sample file as a decimal year.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer         lun
C
C     Returned:
C         Decimal year at midday on the date of observation
              real*8      epoch
C         Status
              integer     s

C     NPR     11 June 1987
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_epoch (epoch, s)
      elseif (ct_vers .eq. 2) then
         call enq_v2_epoch (epoch, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_EPOCH' )

      end
