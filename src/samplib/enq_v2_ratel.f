

C+ENQ_V2_RATEL

       subroutine enq_v2_RAtel( RAtel, s )
C
C     Returns RAtel for V2 sample files
C
C     Returned:
C         RAtel
              real*8          RAtel
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Control tables version 2 support routine for ENQ_RATEL
C-

C     Global includes -
C
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      RAtel  = RAdate + Azlong

      end
