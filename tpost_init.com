!
! Version 2 POSTMORTEM - Test version initialisation file
!
! PA 14/11/89
! DJT 9/7/92   ; GGP 21/10/98, 27/10/98, 1/9/1999
!
! To remove the banner comment out the next line:
!  display-banner
!  display-version
cc Test version - date last linked:
^system cat /mrao/post/tpost_date_link
^system cat /mrao/post/post_new.text

! Define standard procedures
^run-file /mrao/post/util/post_setup.com
set-plot /xterm
