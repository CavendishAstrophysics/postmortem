C+ENQ_SFDIR

       subroutine enq_sfdir ( tscope_name, sf_dir, s )
C
C     Returns the default sample file directory.
C
C     Given:
C         Telescope name.
              character*(*)   tscope_name
C
C     Returned:
C         Sample file directory
              character*(*)   sf_dir
C         Status
              integer         s
C
C     This routine returns the default (directory:user) for the sample
C     files corresponding to a specified telescope name.  This routine
C     should be adjusted appropriately for the CLFST and RYLE systems.
C
C     DJT, 28/10/91
C
C-

      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      if (tscope_name.eq.'T151') then
         sf_dir = '/data4/t151'
      elseif (tscope_name.eq.'38MHZ') then
         sf_dir = '/data4/38mhz'
      elseif (tscope_name.eq.'RYLE') then
         sf_dir = '/data4/ryle'
      else
         s = ILL_TSCOPE
         goto 999
      endif

      return

  999 call smp_wrerr( s, ' in subroutine ENQ_SFDIR' )

      end
