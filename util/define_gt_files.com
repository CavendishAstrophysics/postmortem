! Procedure to define new GT file names
!
! PA 07/02/92
!
^if %ma <%sub-system!NONE> CALIBRATION ^jump do-setting

! this is not the calibration sub-system -- exit
^comment *** Command not available in <%sub-system>
^quit

^loop-marker do-setting
^request 'GT-file : ' '<rt-gt-file>' rt-gt-file
^request 'GTvis-file : ' '<rt-gtvis-file>' rt-gtvis-file
monit-solution set-gt-file <rt-gt-file> <rt-gtvis-file>,,,,
