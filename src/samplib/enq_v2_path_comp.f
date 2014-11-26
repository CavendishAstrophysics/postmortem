c+enq_v2_path_comp

       subroutine enq_v2_path_comp( pc_ra, pc_dec, s )
C
C     Returns the path compensation centre of a sample file.
C
C     Returned:
C         Phase center of path compensators at epoch (I think).
              real*8          pc_ra, pc_dec
C         Status
              integer         s

C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if (s.ne.0) return

      pc_ra  = RAdate
      pc_dec = DECdate

      if (s.ne.0) then
         call smp_wrerr( s, 'in subroutine enq_v2_path_comp.' )
      endif

      end


