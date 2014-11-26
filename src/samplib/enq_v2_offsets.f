


C+ENQ_V2_OFFSETS

       subroutine enq_v2_offsets ( iae, hoff, doff, s )
C
C     Returns the HA and DEC fixed encoder offsets
C
C     Given:
C         Aerial number
              integer     iae
C
C     Returned:
C         HA and DEC encoder offsets
               real       hoff, doff
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_OFFSETS.
C
*-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

       if (iae.ge.1 .and. iae.le.max_aes) then
        hoff = hofs(iae)
        doff = dofs(iae)
      else
        s = ILL_AERIAL
      end if

      end
