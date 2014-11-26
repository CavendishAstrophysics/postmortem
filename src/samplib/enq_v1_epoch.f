


C+ENQ_V1_EPOCH

       subroutine enq_v1_epoch ( epoch, s )
C
C     Returns the epoch as a decimal year from control tables.
C
C     Returned:
C         Decimal year at midday on the date of observation
              real*8      epoch
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_EPOCH.
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if (s .ne. 0) return

      epoch = datobs

      end
