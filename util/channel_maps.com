!
! Procedure to make channel maps
!
^request 'Map-name : ' ' ' %map-name
^assign n 1
^loop-marker loop
  ^calc m <n> 1 +
  set-lsf <m> yes
  size 256,256
  save <%map-name>-CH<n>:map
  make <%map-name>-CH<n>:map,telescope,,
  ^calc n <n> 1 +
^if %ne <n> 8 ^jump loop
^deassign n
^deassign m
^deassign %map-name
