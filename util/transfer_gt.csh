#!/bin/csh -f
#
# Procedure to transfer Ryle Telescope gain tables from ND systems.
#
# This procedure transfers gain tables from one of the ND systems to the
# Sun Unix cluster.  As each file is transferred it is converted if to the
# Unix format.
#
# Parameters may be specified on the command line by keyword or position.
#   -host      ==   remote host (CLFST or RYLE)
#   -name      ==   gain table name
#   -path      ==   path for destination directory
#   -noprompt  ==   flag for no interactive prompts
#
# [DJT, 3/8/95]
#

# Analyse parameters specified on the command line

@ i = 1
while ($i <= $#argv)
   if ( $argv[$i] == '-host' ) then
     @ i = $i + 1
     set rhost = $argv[$i]
   else if ( $argv[$i] == '-name' ) then
     @ i = $i + 1
     set name = $argv[$i]
   else if ( $argv[$i] == '-path' ) then
     @ i = $i + 1
     set lpath = $argv[$i]
   else if ( $argv[$i] == '-noprompt' ) then
     set noprompt
   else if ( $argv[$i] == '-help' ) then
     echo Parameters available are:
     echo "  -host       =  remote host (CLFST or RYLE) "
     echo "  -name       =  name of gain table file "
     echo "  -path       =  destination path "
     echo "  -noprompt   =  non-interactive mode "
     echo ""
     exit
   else if ( ! $?rhost ) then
     set rhost = $argv[$i]
   else if ( ! $?name ) then
     set name = $argv[$i]
   else if ( ! $?lpath ) then
     set lpath = $argv[$i]
   endif
   @ i = $i + 1
end

# Check parameters and prompt for unset values

if ( ! $?rhost ) then
  set  rhost = RYLE
  if ( ! $?noprompt ) then
    echo -n "remote host (CLFST or RYLE): "
    set rhost = ($<)
  endif
endif
if ( $rhost =~ [cC][lL][fF][sS][tT] ) set rhost = CLFST
if ( $rhost =~ [rR][yY][lL][eE] )     set rhost = RYLE

while ( ! $?name )
  set name = "RT-GT:DATA"
  if ( ! $?noprompt ) then
    echo -n "name of gain table file [" $name "] : "
    set a = ($<)
    if ( $#a > 0 ) set name = $a
  endif
end

if ( ! $?lpath ) then
  set lpath = /mrao/post/data
  if ( ! $?noprompt ) then
    echo -n "destination ($lpath): "
    set a = ($<)
    if ( $#a > 0 ) set lpath = $a
  endif
endif

# Get transfer list from ND host

set files = `/mrao/post/util/transfer_gt.exp $rhost $name | grep \(`
set files = `echo $files | tr -d '\015' ''`
set files = `echo $files | awk '{for (i=4; i<=NF; i=i+4) printf " %s",$i }'`
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

if ( $?noprompt ) then
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

set file = `echo $i | sed -n s/\)/\;/p | awk -F\; '{print $2}'`
set file = `echo $file | tr '[A-Z]-:' '[a-z]_.'`

set lfile = $lpath/$file
echo copy $i as $lfile

ftp -n $rhost <<end2
user POSTMORTEM TELESCOPE
binary
get $i $lfile
quit
end2

if ( -f $lfile ) then
  if ( `echo $lfile | grep -i vis` != "") then
    /mrao/post/util/convert_gtvis $lfile
  else
    /mrao/post/util/convert_gt $lfile
  endif
endif

end

echo transfer completed: `date`

end:

echo ""

