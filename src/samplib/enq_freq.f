

C+ENQ_FREQ

       subroutine enq_freq ( lun, freq, s )
C
C     Returns the observing frequency of a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              integer     lun
C
C     Returned:
C         Observing frequency (Hz).
               real*8      freq
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
         call enq_v1_freq( freq, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_freq( freq, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_FREQ' )

      end
