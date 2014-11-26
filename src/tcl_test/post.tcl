#
# Initialisation file for Postmortem X and non-X versions
# 
#
global Post Anmap
global PostSource Post_UserDir Desktop auto_path

# define search paths
set Anmap(src) /mrao/anmap_v7.5
set PostSource /mrao/post/src/tcl_test
set Post_UserDir $env(HOME)/mrao
set Post(sysOptions) anmaprc
set Post(userOptions) ~/.postrc
set Post(src) $PostSource
set Post(lib) $PostSource
set Post(UserDir) $Post_UserDir

# setup map directory environment and sample file environment
set env(MAPSDIR) $env(HOME)/images
set env(SAMPDIR) /mraosc/data/ryle

# setup search paths etc.
set src /soft/tcl/ndt ; lappend auto_path $src/lib
if $Xpost then {
  Desktop_Init Post $src -command Post
} else {
  Desktop_Init Post $src -command Post -noX
}
lappend auto_path $Post(src)/tcl_lib

# define static Post globals
set Post(Ted) {}

# define Post version number
set Post(Version) X0.2
set Post(version) X0.2
set Post(date) 5/08/94

# load default desktop initialisation file
if $Xpost then {
  if [file exists ~/.wishrc] then {source ~/.wishrc}
}

# make available standard commands
source $Post(src)/standard_init
source $Post(src)/standard_info

# initialise postmortem
post_init 

# initialise the interpreter
iocmd clear-cli

# startup windows version if required
if $Xpost then {
  Post_ControlPanel

  # grab and re-parent window
  if [info exists env(POST_GRAB)] then {
    tksteal .xterm -command {} -name Xpost -width 80 -height 20 \
                   -borderwidth 0 -relief flat -background grey
    pack append . .xterm {top fill expand}
  }
  wm grid .
  wm maxsize . 1000 1000
}


# run local initialisation file if present
if [file exists $Post(src)/init_post.tcl] then {
  source  $Post(src)/init_post.tcl
}
