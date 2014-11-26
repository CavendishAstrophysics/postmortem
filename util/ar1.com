! set flag, cal (no 1); run archive command
! 31 Jan 2002

lsf
sel-flag 0,1
sel-cal  yes
archive
all






