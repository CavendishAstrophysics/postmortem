#!/bin/csh -f
#
# Procedure to transfer observation files from ND systems.
#
# This procedure transfers all files associated with a given observation
# from one of the ND systems to the Sun Unix cluster.  As each file is
# transferred it is converted if necessary to the Unix format.
#
# The list of files to transfer should be prepared in advance by running
# (e.g.) LIST-OBS-FILES on the remote system.
#
# Parameters may be specified on the command line by keyword or position.
#   -host      ==   remote host (CLFST or RYLE)
#   -telescope ==   telescope (T151 or RYLE)
#   -title     ==   title of observation
#   -save      ==   savelist file name
#   -path      ==   path for destination directory
#   -noprompt  ==   set for no interactive prompting
#
# [DJT, 5/2/96]
#

set def_host = "RYLE"
set def_savelist = "SAVE-SAMP:LIST"

# Analyse parameters specified on the command line

@ i = 1
while ($i <= $#argv)
   if ( $argv[$i] == '-host' ) then
     @ i = $i + 1
     set rhost = $argv[$i]
   else if ( $argv[$i] == '-telescope' ) then
     @ i = $i + 1
     set telescope = $argv[$i]
   else if ( $argv[$i] == '-title' ) then
     @ i = $i + 1
     set title = $argv[$i]
   else if ( $argv[$i] == '-save' ) then
     @ i = $i + 1
     set savelist = $argv[$i]
   else if ( $argv[$i] == '-path' ) then
     @ i = $i + 1
     set lpath = $argv[$i]
   else if ( $argv[$i] == '-noprompt' ) then
     set noprompt
   else if ( $argv[$i] == '-help' ) then
     echo Parameters available are:
     echo "  -host       =  remote host (CLFST or RYLE) "
     echo "  -telescope  =  telescope name (T151 or RYLE) "
     echo "  -title      =  observation title "
     echo "  -save       =  savelist filename "
     echo "  -path       =  destination path "
     echo "  -noprompt   =  non-interactive mode "
     echo ""
     exit
   else if ( ! $?rhost ) then
     set rhost = $argv[$i]
   else if ( ! $?telescope ) then
     set telescope = $argv[$i]
   else if ( ! $?title ) then
     set title = $argv[$i]
   else if ( ! $?lpath ) then
     set lpath = $argv[$i]
   endif
   @ i = $i + 1
end

# Check parameters and prompt for unset values

if ( ! $?rhost ) then
  set rhost = $def_host
  if ( ! $?noprompt) then
    echo -n "remote host [$rhost]: "
    set a = ($<)
    if ( $#a > 0 ) set rhost = $a
  endif
endif
if ( $rhost =~ [cC][lL][fF][sS][tT] ) set rhost = CLFST
if ( $rhost =~ [rR][yY][lL][eE] )     set rhost = RYLE

if ( ! $?telescope ) then
  if ( $rhost =~ [cC][lL][fF][sS][tT] ) set telescope = T151
  if ( $rhost =~ [rR][yY][lL][eE] )     set telescope = RYLE
  if ( ! $?noprompt) then
    echo -n "telescope name [${telescope}]: "
    set a = ($<)
    if ( $#a > 0 ) set telescope = $a
  endif
endif
if ( $telescope =~ [tT]151 )          set telescope = T151
if ( $telescope =~ [rR][yY][lL][eE] ) set telescope = RYLE

if ( ! $?noprompt) then
  if ( ! $?title ) then
    echo -n "observation title: "
    set a = ($<)
    if ( $#a > 0 ) then
      set title = $a
    else if ( ! $?savelist ) then
      set savelist = "($telescope)$def_savelist"
      echo -n "save list [$savelist]: "
      set a = ($<)
      if ( $#a > 0 ) set savelist = $a
    endif
  endif
endif

if ( ! $?lpath ) then
# if ( $?SAMPDIR ) then
#   set lpath = `echo $SAMPDIR`
# else
    if ( $telescope == 'T151' ) set lpath = /data/clfst
    if ( $telescope == 'RYLE' ) set lpath = /data/ryle
# endif
  if ( ! $?noprompt) then
    echo -n "destination [$lpath]: "
    set a = ($<)
    if ( $#a > 0 ) set lpath = $a
  endif
endif

# Get transfer list from ND host

if ( $?title ) then
  set files = `/mrao/post/util/transfer_sf.exp $rhost $telescope $title | grep \(`
  set files = `echo $files | tr -d '\015' ''`
else if ( $?savelist ) then
  ftp -n $rhost <<end1
  user $telescope TELESCOPE
  get $savelist /tmp/save_list
  quit
end1
set files = `cat /tmp/save_list | awk '{print $NF}' | awk -F\; '{print $1}'`
/usr/bin/rm -f /tmp/save_list
endif
  
if ( $#files == 0 ) then
  echo "*** no files found"
  goto end
endif

# Display files to be transferred and prompt for confirmation

start: 

echo ""
echo "The following files will be transferred:"
foreach i ($files)
  echo $i
end

if ( $?noprompt) then
  goto begin
endif

echo -n "Do you want to make a selection from this list [n]? "
set b = `echo $< | cut -c1`
if (("$b" == "y")||("$b" == "Y")) then
  set selection = ""
  foreach i ($files)
    echo -n "$i [y]? "
    set c = `echo $< | cut -c1`
    if (($#c == 0)||("$c" == "y")||("$c" == "Y")) then
      set selection = "$selection $i"
    endif
  end
  set files = "$selection"
  if ("$files" != "") then
    goto start
  endif
endif

while (1)
  echo -n "Do you want to continue [y or n]? "
  set a = `echo $< | cut -c1`
  if (("$a" == "n")||("$a" == "N")) then
    goto end
  else if (("$a" == "y")||("$a" == "Y")) then
    break
  endif
end

begin:

# Begin transfer, converting each file to Unix format as it is received

echo transfer started: `date`

foreach i ($files)

set name = `echo $i | sed -n s/\)/:/p | awk -F: '{print $3}'`
set type = `echo $i | sed -n s/\)/:/p | awk -F: '{print $4}'`
set type = `echo $type | awk -F\; '{print $1}' | tr '[A-Z]' '[a-z]'`

set ldir = $lpath/$name
if ( ! -d $ldir) then
  mkdir $ldir
endif

set lfile = $ldir/$type
echo copy $i as $lfile

ftp -n $rhost <<end2
user $telescope TELESCOPE
binary
get $i $lfile
quit
end2

if ( -f $lfile ) then
  if ( $type == cal ) then
    /mrao/post/util/convert_sf $lfile
  else if ( $type == ion ) then
    /mrao/post/util/convert_ion $lfile
  else if ( $type == lsf ) then
    /mrao/post/util/convert_lsf $lfile
  else if ( $type == rem ) then
    /mrao/post/util/convert_sf $lfile
  else if ( $type == samp || \
            $type == s5   || $type == s15 || \
            $type == s27  || $type == s31 ) then
    /mrao/post/util/convert_sf $lfile
  endif
endif

chmod -R g+w $ldir
chmod g+s $ldir

end

echo transfer completed: `date`

end:

echo ""

