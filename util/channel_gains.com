^assign n 1
^loop-marker loop
display-samp
ch <n>
1,,,,,
^request '.. press return to continue' ' ' ans
^deassign ans
^calc n <n> 1 +
^if %ne <n> 9 ^jump loop
