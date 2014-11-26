! retrospective save of .out files:
! 2nd cal; flag; current dir, long int.


cc 26 April 2001


^get variable sd SAMPDIR
! cc Sample-file diretory is <sd>
^assign aelist 1-5

^loop-marker START
^request 'name of sample file : '               '<sf>' sf

! testing <sd>/<sf>

^if %file_exists(<sd>/<sf>) ^jump SSF1

cc <sd>/<sf> does not exist ...
^jump START

^loop-marker SSF1
set-sample-file <sf>

set-display cos

lsf
select-flag 0 1
select-cal
no
yes

cc merging ...
merge
total
all
1,,,

^loop-marker L2

^request 'print-spacings ?', 'yes', printsp
^if %match (<printsp>, no) ^jump NOCALPRINT

^assign fileowner /home/guy/var/

^if %match (3C6, <sf>)      ^assign fileowner /home/guy/3C6/
smooth 1000 1000


^request 'output file :'  '<fileowner><sf>.out' outfile
print-spacing
total
all
1,6000
<outfile>

^loop-marker NOCALPRINT

^request 'more fields ?', 'yes', more
^if %match (<more>, YES) ^jump START


^quit
