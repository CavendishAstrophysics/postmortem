



C+ENQ_V1_ZFILE

       subroutine enq_v1_zfile ( file, s )
C
C     Returns the zero-correction file name from control tables.
C
C     Returned:
C         Name of zero-correction data file.
              character*(*)   file
C         Status
              integer         s
C
C     Control tables version 0 support routine for ENQ_io_namfil.
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'

      if ( s .ne. 0 ) return

      file = zfile(1:lzf)

      end
