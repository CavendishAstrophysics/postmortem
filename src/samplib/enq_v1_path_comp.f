
C+ENQ_V1_PATH_COMP

       subroutine enq_v1_path_comp( pc_ra, pc_dec, s )
C
C     Returns the path compensation centre of a sample file.
C
C     Returned:
C         Phase center of path compensators at epoch (I think).
              real*8          pc_ra, pc_dec
C         Status
              integer         s

*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'

      if ( s .ne. 0 ) return

      pc_ra  = rapc
      pc_dec = decpc

      if (s.ne.0) call io_wrerr( s, 'in subroutine enq_v1_path_comp.' )

      end
