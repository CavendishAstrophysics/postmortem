!
! Procedure to define channel LSFs
!
^assign n 1
^loop-marker loop
  spacing-list
  ch <n>
  not-spac
  ae 2/4
  save Map Channel-<n>
  ^ccomment .. LSF for Channel <n> saved
  ^calc n <n> 1 +
^if %ne <n> 8 ^jump loop
^deassign n
