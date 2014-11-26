cc calibrate with interleaved cal source - default ae 1-5

! 18 Feb 97; 19 Feb 98; 1 April 98; 19 May 98
! 27 Oct 98 [unix verson], 11 May 99
cc 13 Apr 2000

! ^assign sd /data/ryle
^get variable sd SAMPDIR
cc sample-file directory is <sd>

^loop-marker START
^request 'name of cal    source : '            '<SZcal>' SZcal
^request 'name of target source : '            '<SZtar>' SZtar

^request 'date of observations (as YYMMDD) : ' '<SZdate>' SZdate

^if %file_exists(<sd>/<SZtar>-<SZdate>) ^jump SSF1

cc <sd>/<SZtar>-<SZdate> does not exist ...
^jump START

^loop-marker SSF1
set-sample-file <SZtar>-<SZdate>

^if %file_exists(<sd>/<SZcal>-<SZdate>) ^jump SSF2

cc <sd>/<SZcal>-<SZdate> does not exist ...
^jump START

^loop-marker SSF2
set-sample-file <SZcal>-<SZdate>

^request 'calibration number : '               '<cal_SZ>' cal_SZ
^request 'aerial list : '                      '1-5'      aelist

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

^assign flag5D yes
^request 'reference aerial : '                 <refant!5> refant
^request 'flag out shadowed data?'             'yes'      shadow
! ^request 'flag out 5C?'                        '<flag5C!no>'   flag5C
^request 'flag out 5D?'                        '<flag5D!no>'   flag5D
! ^request 'flag out 6C?'                        '<flag6C!no>'   flag6C
! ^request 'flag out 6D?'                        '<flag6D!no>'   flag6D
^assign flag5C no
^assign flag6C no
^assign flag6D no

! get back to top level ... else attempt to delete cal/lsf may fail
Monitor
Q

cc delete any previous calibration and lsf [not flags]


^if %file_exists (<sd>/<SZcal>-<SZdate>/cal) ^system /usr/bin/rm <sd>/<SZcal>-<SZdate>/cal
^if %file_exists (<sd>/<SZcal>-<SZdate>/lsf) ^system /usr/bin/rm <sd>/<SZcal>-<SZdate>/lsf
^if %file_exists (<sd>/<SZtar>-<SZdate>/cal) ^system /usr/bin/rm <sd>/<SZtar>-<SZdate>/cal
^if %file_exists (<sd>/<SZtar>-<SZdate>/lsf) ^system /usr/bin/rm <sd>/<SZtar>-<SZdate>/lsf


cc create <SZcal> CAL file ...
calibration
update read <cal_SZ>
<aelist>
apply  [<cal_SZ>]  ae <aelist>

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

^if  %match (<flag5C>, No) ^jump F5C1
lsf
flag add 0
ae 5; sb C
1-6000
set
-
^loop-marker F5C1

^if  %match (<flag5D>, No) ^jump F5d1
lsf
flag add 0
ae 5; sb D
1-6000
set
-
^loop-marker F5d1
^if  %match (<flag6c>, No) ^jump F6c1
lsf
flag add 0
ae 6; sb C
1-6000
set
-
^loop-marker F6c1
^if  %match (<flag6D>, No) ^jump F6D1
lsf
flag add 0
ae 6; sb D
1-6000
set
-
^loop-marker F6D1



cc look at the wind-speed and rain gauge data
monitor
check-wind,y,,,,
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
^request 'smoothing for cal source (samples) :' '5'      smooth

cc save calibration lsf ...
lsf
smooth <smooth> <smooth>
select-flag 0,1
select-cal
yes
save cal: [<cal_SZ>] ae <aelist> sm <smooth> <smooth> flag 0,1

cc phase self-cal for <SZcal> ...
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
! cc and scan the merged data for <SZcal> ....
! lsf
! sel-cal no yes
! sel-flag 0,1
! smooth 1 1
! spacing ae <aelist>/<aelist>
! scan-sample-file
! total yes yes
! ae <aelist>/<aelist>
! 1,,,
! 1000
cc plot merged spacings for cal source ...
lsf
sel-cal no yes
sel-flag 0,1
smooth 1 1
spacing ae <aelist>/<aelist>
merge
total
ae <aelist>/<aelist>
1,,,

set-sample-file <SZtar>-<SZdate>
cc flag the data for the <SZtar> ...
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



^if  %match (<flag5C>, No) ^jump F5C2
lsf
flag add 0
ae 5; sb C
1-6000
set
-
^loop-marker F5C2

^if  %match (<flag5D>, No) ^jump F5d2
lsf
flag add 0
ae 5; sb D
1-6000
set
-
^loop-marker F5d2
^if  %match (<flag6c>, No) ^jump F6c2
lsf
flag add 0
ae 6; sb C
1-6000
set
-
^loop-marker F6c2
^if  %match (<flag6D>, No) ^jump F6d2
lsf
flag add 0
ae 6; sb D
1-6000
set
-
^loop-marker F6d2


cc now make calibration for <SZtar> ... only reporting closure errors > 25 deg
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
q
^request 'save the LSF for writing FITS data ?', '<fitsit>', fitsit
^if %match (<fitsit>,  NO)  ^jump NOFITS
cc save the LSF for FITS data ...
lsf
^request 'smoothing for FITSwrite (samples) :' '<fitsmooth>'   fitsmooth
^request 'int chop [pre]  :' '<prechop>'  prechop
^request 'int chop [post] :' '<postchop>' postchop

sel-flag 0,1
sel-cal yes
sp ae <aelist>/<aelist>
smooth <fitsmooth> <fitsmooth>
interference-chop
yes clip-cos-sin <prechop>
yes clip-cos-sin <postchop>
save fits: ae <aelist> cal, flag 0,1  sm <fitsmooth>  int chops <prechop>,<postchop>


Q
^loop-marker NOFITS
^quit
