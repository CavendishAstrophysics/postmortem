!
! Version 2 POSTMORTEM - Standard Initialisation file
!
! PA 14/11/89
! DJT 9/7/92   ; GGP 21/10/98, 27/10/98
!
! To remove the banner comment out the next line:
!  display-banner
!  display-version
cc Date last linked:
^system cat /mrao/post/post_date_link
^system cat /mrao/post/post_new.text

!
! Turn communication system on
! batch-monitor receive-communication on
!
! Define standard procedures
^run-file /mrao/post/util/post_setup.com
set-plot /xterm
