
cc calibrate & phase self-cal single source observation

cc 18 Aug 2005

! derived from cal-auto, so source is called 'calsource'


^get variable sd SAMPDIR
! cc Sample-file diretory is <sd>
^assign aelist 1-5

^loop-marker START
^request 'name of source : '               '<calsource>' calsource
^request 'date of observations (eg 030901) : ' '<date>'      date

! testing <sd>/<calsource>-<date>

^if %file_exists(<sd>/<calsource>-<date>) ^jump SSF1

cc <sd>/<calsource>-<date> does not exist ...
^jump START

^loop-marker SSF1
set-sample-file <calsource>-<date>
^request 'calibration number : '               '<calno>'   calno
^request 'aerial list : '                      '<aelist>'  aelist

^if %match (<aelist>, 1-6) ^assign refant 5
^if %match (<aelist>, 1-5) ^assign refant 5
^if %match (<aelist>, 1-4) ^assign refant 4
^if %match (<aelist>, 1-3) ^assign refant 3
^if %match (<aelist>, 1-2) ^assign refant 2
^if %match (<aelist>, 2-5) ^assign refant 5
^if %match (<aelist>, 2-4) ^assign refant 4
^if %match (<aelist>, 2-3) ^assign refant 3
^if %match (<aelist>, 3-5) ^assign refant 5
^if %match (<aelist>, 3-4) ^assign refant 4

^request 'reference aerial : '                 '<refant>' refant
^request 'flag out shadowed data?'             'yes'     shadow


^assign gen_flag no

^assign flag6D no
^if %match (<aelist>, 1-6)                ^assign flag6D yes
^assign flagB no
^assign flag5C  no
^assign flag5D  No
^assign flagD   no
^assign flagE   No

^request 'flag out specified spacings?'        '<gen_flag>'  gen_flag
^if %match (<gen_flag>, no) ^jump NO_gen_flag

! ^request-text  'list of spacings:' '<gen_list>' gen_list
^request  'list of spacings - enclose in single quotes:' '<gen_list>' gen_list

^loop-marker NO_gen_flag

! get back to top level ... else attempt to delete cal/lsf fails
Monitor
Q

cc deleting any previous calibration and lsf
^if %file_exists (<sd>/<calsource>-<date>/cal) ^system /usr/bin/rm <sd>/<calsource>-<date>/cal
^if %file_exists (<sd>/<calsource>-<date>/lsf) ^system /usr/bin/rm <sd>/<calsource>-<date>/lsf
^request 'delete earlier flag files?' 'no', delflag
^if %match (<delflag>, no) ^jump nodelflag1
^if %file_exists (<sd>/<calsource>-<date>/flag) ^system /usr/bin/rm <sd>/<calsource>-<date>/flag
^loop-marker nodelflag1
cc create <calsource> CAL file ...
set-sample-file <calsource>-<date>
calibration
update read <calno>
<aelist>
apply  [<calno>]  ae <aelist>

cc flag the calibrator data ...

monitor
^if  %match (<shadow>, yes) ^jump s1
^jump s2
^loop-marker s1
shadow
yes
^loop-marker s2

flag,,,
<aelist>
1,,,
yes no


^if  %match (<gen_flag>, no) ^jump no_gen_flag_1
lsf
flag add 0
<gen_list>
1-6000
set
-

^loop-marker no_gen_flag_1

^if  %match (<flagB>, No) ^jump FB1
lsf
flag add 0
sb B
1-6000
set
-
^loop-marker FB1

^if  %match (<flag5C>, No) ^jump F5c1
lsf
flag add 0
ae 5; sb C
1-6000
set
-
^loop-marker F5c1
^if  %match (<flag5D>, No) ^jump F5D1
lsf
flag add 0
ae 5; sb D
1-6000
set
-
^loop-marker F5D1

^if  %match (<flagE>, No) ^jump FD1
lsf
flag add 0
sb E
1-6000
set
-
^loop-marker FD1
^if  %match (<flag6D>, No) ^jump F6D1
lsf
flag add 0
ae 6; sb D
1-6000
set
-
^loop-marker F6D1

monitor
cc wind statistics
check-wind,y,,,,
cc look at the rain gauge data
check-rain-gauge
no,yes,no
1,,,
check-rain-gauge
no,no,yes
<refant>
1,,,


set-display amp-phase
^request  'plot one spacing of the source?' 'Yes' calplot
^if  %match (<calplot>, Yes) ^jump L1
cc ... no plotting
^jump L2
^loop-marker L1
^assign plotlist 1/5
^if %match (<aelist>, 1-6) ^assign plotlist '1/6'
^if %match (<aelist>, 1-5) ^assign plotlist '1/5'
^if %match (<aelist>, 1-4) ^assign plotlist '1/4'
^if %match (<aelist>, 1-3) ^assign plotlist '1/3'
^if %match (<aelist>, 1-2) ^assign plotlist '1/2'
^if %match (<aelist>, 2-5) ^assign plotlist '2/5'
^if %match (<aelist>, 2-4) ^assign plotlist '2/4'
^if %match (<aelist>, 2-3) ^assign plotlist '2/3'
^if %match (<aelist>, 3-5) ^assign plotlist '3/5'
^if %match (<aelist>, 3-4) ^assign plotlist '3/4'

^request  'ae pair to plot : '    <plotlist> plotlist
cc ... plotting merge of ae <plotlist> sb A

lsf
select-flag 0 1
select-cal
yes
spacing ae <aelist>/<aelist>

merge
total
sb A; ae <plotlist>
1,,,

^loop-marker L2
^assign  smooth  5
^request 'smoothing for phase self-cal (samples) :' '<smooth>'      smooth

cc save calibration lsf 
lsf
smooth <smooth> <smooth>
select-flag 0,1
select-cal
yes
spacing ae <aelist>/<aelist>
save cal: [<calno>] ae <aelist> sm <smooth> <smooth> flag 0

cc phase self-cal for <calsource> ...
calibration
solution check-closure 10
amplitude off
reference <refant>
model
point
1


1
no
no
no
no
chan


save
go
cc and plot the merged data for <calsource> [flag 0,1] ...

lsf
sel-cal no yes
sel-flag 0,1
smooth 1 1
spacing ae <aelist>/<aelist>
merge
total
ae <aelist>/<aelist>
1,,,

^request 'archive flux-density ?' 'yes' arc
^if %match (<arc>, no) ^jump noarc
archive-flux
all
1,,,

^loop-marker noarc



^loop-marker MOREFIELDS
^request 'more fields ?', 'no', more
^if %match (<more>, YES) ^jump START

cc ... finishing ...


! ^quit
