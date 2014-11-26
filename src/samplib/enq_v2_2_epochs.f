


C+ENQ_V2_2_EPOCHS

       subroutine enq_v2_2_epochs (e_ref, e_obs, s)
C
C     Returns two epochs as decimal years from control tables.
C
C     Returned:
C         Ref epoch; dec year at midday on the date of observation
              real*8      e_ref, e_obs
C         Status
              integer     s
C
C     Control tables version 2 support routine for ENQ_2_EPOCHS
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v2.inc'

      if (s .ne. 0) return

      e_ref = datref
      e_obs = datobs

      end
