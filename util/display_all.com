!
! DISPLAY-ALL:   Procedure to display data from all Aerial Pairs
!                output is ordered by increasing aerial pairs
!                (i.e. all spacings for ae 1 then ae 2 etc.)
!                Spacings are only displayed once
!
! PA 28/9/89; GP 28/10/98
!
^request 'Number of Aerials : ' '5' numa
^calc numb <numa> 1 -
^request 'Display type (channel, spacings) : ' 'Channel' type
^assign n1 0
^loop-marker loop1
  ^calc n1 <n1> 1 +
  ^assign n2 <n1>
  ^loop-marker loop2
    ^calc n2 <n2> 1 +
    display-<type>
    ae <n1> / <n2>
    ,,,,,,,,
    ^request '.. press return to continue (Q=quit) : ' ' ' ans
    ^if %match (<ans>, Q) ^jump exit
  ^if %lt (<n2>, <numa>) ^jump loop2
^if %lt (<n1>, <numb>) ^jump loop1
!
! tidy up all variables before exit
!
^loop-marker exit
^deassign n1
^deassign n2
^deassign numa
^deassign numb
^deassign type
^deassign ans
