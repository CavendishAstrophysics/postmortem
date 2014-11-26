! set flag, cal (no 2); run archive command
! 31 Jan 2002

lsf
sel-flag 0,1
sel-cal no, yes
archive
all





