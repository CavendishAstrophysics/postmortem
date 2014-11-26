


C+ENQ_2_EPOCHS

       subroutine enq_2_epochs (lun, e_ref, e_obs, s)
C
C     Returns the reference and observation epochs as decimal years
C
C       given: sample-file log unit no
              integer     lun
C     Returned: reference and observation epochs (years)
              real*8      e_ref, e_obs
C         Status
              integer     s
C
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if (s .ne. 0) return

      call read_ct(lun, s)
      if (s .ne. 0) goto 999

      if     (ct_vers .le. 1) then
         call enq_v1_2_epochs (e_ref, e_obs, s)
      elseif (ct_vers .eq. 2) then
         call enq_v2_2_epochs (e_ref, e_obs, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr(s, 'in subroutine ENQ_2_EPOCHS')

      end
