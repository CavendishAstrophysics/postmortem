


C+ENQ_V1_2_EPOCHS

       subroutine enq_v1_2_epochs (e_ref, e_obs, s)
C
C     Returns the ref and observation epochs as decimal years from
C     control tables.
C
C     Returned:
C         ref date; dec year at midday on the date of observation
              real*8      e_ref, e_obs
C         Status
              integer     s
C
C     Control tables version 1 support routine for ENQ_2_EPOCHS
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v1.inc'

      if (s .ne. 0) return

      e_ref = datref
      e_obs = datobs

      end
