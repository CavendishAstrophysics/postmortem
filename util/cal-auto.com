
cc calibrate multi-source observation with interleaved cal  -  default ae 1-5

! 18 May 98, 12 Oct 98, 29 Oct 98 (ND)
! 11 March 99 ...

cc 24 April 2006



^get variable sd SAMPDIR
! cc Sample-file diretory is <sd>
^assign aelist 1-5

^loop-marker START
^request 'name of cal source : '               '<calsource>' calsource
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
^if %match (<aelist>, 1-5) ^assign refant 1
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
^assign flag5D  Yes
^assign flagD   no
^assign flagE   No


! ^request 'flag out sb B?'                      '<flagB>' flagB
! ^request 'flag out 5C?'                        '<flag5C>' flag5C
^request 'flag out 5D?'                        '<flag5D>' flag5D
! ^request 'flag out sb E?'                      '<flagE>' flagE
! ^request 'flag out 6D?'                        '<flag6D>' flag6D

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
^request  'plot one spacing of the calibrator?' 'Yes' calplot
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
^if %match  (<calsource>, 1920154)       ^assign smooth  20
^request 'smoothing for cal source (samples) :' '<smooth>'      smooth

cc save calibration lsf ... note flag 0 only
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

^request 'archive flux-density of cal source ?' 'yes' arc
^if %match (<arc>, no) ^jump noarc
archive-flux
all
1,,,

^loop-marker noarc

! ^request 'print-spacings for CAL ?', 'no', printsp
! ^if %match (<printsp>, no) ^jump NOCALPRINT

! ^assign fileowner /home/guy/var/

! ^if %match (<calsource>, 3C6)      ^assign fileowner /home/guy/3C6/
! smooth 6000 6000
! ^request 'output file :'  '<fileowner><calsource>-<date>.out' outfile
! print-spacing
! total
! ae <aelist>/<aelist>
!  1,1
! <outfile>
! smooth 1 1

^loop-marker NOCALPRINT



!! ^if %match (<calsource>, 2005403)  ^assign target CYGNUSX3
!! ^if %match (<calsource>, 2005403A) ^assign target CYGNUSX1
!! ^if %match (<calsource>, 2005403B) ^assign target CYGNUSX3
!! ^if %match (<calsource>, 1920154)  ^assign target 1915105
!! ^if %match (<calsource>, 4C6705)   ^assign target LSI61303
!! ^if %match (<calsource>, BLLAC)    ^assign target CYGNUSX2

^loop-marker MAIN

cc

lsf
q


next-sample-file

^file open /tmp/RT-sample-file.list READ unit
^file read  <unit> fullname
^file read  <unit> target
^file close <unit>

cc target is <target>

^if %file_exists(<sd>/<target>-<date>) ^jump SSF2

cc <sd>/<target>-<date> does not exist ...
^jump MAIN


^loop-marker SSF2
^if %file_exists (<sd>/<target>-<date>/cal) ^system /usr/bin/rm <sd>/<target>-<date>/cal
^if %file_exists (<sd>/<target>-<date>/lsf) ^system /usr/bin/rm <sd>/<target>-<date>/lsf
^if %match (<delflag>, no) ^jump nodelflag2
^if %file_exists (<sd>/<target>-<date>/flag) ^system /usr/bin/rm <sd>/<target>-<date>/flag
^loop-marker nodelflag2

cc flag the data for the <target> ...
monitor
flag,,,
<aelist>
1,,,
yes no
^if  %match (<shadow>, yes) ^jump s3
^jump s4
^loop-marker s3
shadow
yes
^loop-marker s4

^if  %match (<gen_flag>, no) ^jump no_gen_flag_2
lsf
flag add 0
<gen_list>
1-6000
set
-
^loop-marker no_gen_flag_2

^if  %match (<flagB>, No) ^jump FB2
lsf
flag add 0
sb B
1-6000
set
-
^loop-marker FB2

^if  %match (<flag5c>, No) ^jump F5c2
lsf
flag add 0
ae 5; sb C
1-6000
set
-
^loop-marker F5c2
^if  %match (<flag5D>, No) ^jump F5d2
lsf
flag add 0
ae 5; sb D
1-6000
set
-
^loop-marker F5d2

^if  %match (<flagE>, No) ^jump FD2
lsf
flag add 0
sb E
1-6000
set
-
^loop-marker FD2

^if  %match (<flag6D>, No) ^jump F6d2
lsf
flag add 0
ae 6; sb D
1-6000
set
-
^loop-marker F6d2


cc now make calibration
calibration
solution check-closure 25
cal-file
yes yes

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


cc
cc and plot the merged (cos & sine) data for the target source ....

set-display cos-sin
lsf
sel-cal yes
sel-flag 0,1
smooth 1 1
spacing ae <aelist>/<aelist>
merge
total
ae <aelist>/<aelist>
1,,,

set-display amp-phase

^request 'print-spacings ?', 'no', printsp
^if %match (<printsp>, no) ^jump NOPRINT

^assign fileowner /home/guy/var/
^if %match (<target>, CYGNUSX3)   ^assign fileowner /home/guy/cx3/
^if %match (<target>, CYGNUSX1)   ^assign fileowner /home/guy/cx1/
^if %match (<target>, 1915105)    ^assign fileowner /home/guy/1915/
^if %match (<target>, J1118480A)  ^assign fileowner /home/guy/J1118480/
^if %match (<target>, IIPEG)      ^assign fileowner /home/guy/iipeg/
^if %match (GRB, <target>)        ^assign fileowner /home/guy/grb/
^if %match (<target>, SS433)      ^assign fileowner /home/guy/SS433/
^if %match (<target>, LSI61303)   ^assign fileowner /home/guy/lsi61303/

^request 'first sample :' '1',    firstsamp
^request 'last  sample :' '6000', lastsamp
! samp <firstsamp>-<lastsamp>

^request 'output file :'  '<fileowner><target>-<date>.out' outfile

smooth 1 1

print-spacing
total
ae <aelist>/<aelist>
<firstsamp>
<lastsamp>
<outfile>

! samp 1-6000
^loop-marker NOPRINT


^request 'archive flux-density ?' 'yes' arct
^if %match (<arct>, no) ^jump noarct
^request 'first sample :' '1',    firstsamp
^request 'last  sample :' '6000', lastsamp

archive-flux
all
<firstsamp>
<lastsamp>

^loop-marker noarct


^request 'map ?', '<map>', map
^if %match (<map>, yes)  ^jump MAP
^jump TAIL

^loop-marker MAP
save map ae <aelist>/<aelist>
map
size 128 128
proj

6
equatorial

type map
save <target>-<date>.map
type beam
save <target>-<date>.beam
type map
make,,

q


^loop-marker TAIL
^assign more ??

^request 'more sources ?', '<more>', more

^if %match (<more>, YES) ^jump NEWTAR
^if %match (<more>, NO)  ^jump MOREFIELDS
^jump TAIL

^loop-marker NEWTAR
^if %match (<target>, CYGNUSX1) ^assign target temp-cyg
^if %match (<target>, CYGNUSX3) ^assign target CYGNUSX1
^if %match (<target>, temp-cyg) ^assign target CYGNUSX3

^jump MAIN

^loop-marker MOREFIELDS
^request 'more fields ?', 'no', more
^if %match (<more>, YES) ^jump START

cc ... finishing ...


! ^quit
