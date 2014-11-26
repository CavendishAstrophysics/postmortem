cc calibrate with interleaved cal source - default ae 1-5
cc optional automatic flagging for target source only (rasters)

! 18 Feb 97; 19 Feb 98; 1 April 98; 19 May 98
! 27 Oct 98 [unix verson], 8 Dec 98; 24 Sep 99; 13 Oct 99
cc 13 Apr 2000

^assign sampdir /data/ryle
^assign sd      /data/ryle
! cc sample-file directory is <sampdir>

^loop-marker START
^request 'name of target source : '            '<RAStar>' RAStar
^request 'name of cal    source : '            '<RAScal>' RAScal
^request 'date of observations [eg 991013] : ' '<RASdate>' RASdate

^if %file_exists(<sd>/<RAStar>-<RASdate>) ^jump SSF1

cc <sd>/<RAStar>-<RASdate> does not exist ...
^jump START

^loop-marker SSF1

^if %file_exists(<sd>/<RAScal>-<RASdate>) ^jump SSF2

cc <sd>/<RAScal>-<RASdate> does not exist ...
^jump START

^loop-marker SSF2

set-sample-file <RAScal>-<RASdate>

^request 'calibration number : '               '<cal_RAS>' cal_RAS
^request 'aerial list : '                      '1-5'      aelist
^request 'reference aerial : '                 <refant!5> refant
^request 'flag out shadowed data?'             'yes'      shadow
^request 'flag by pointing error for target ?' 'yes'      autoflag
^if %match (<autoflag>, yes) ^request 'error gate :' '50'  errorgate

^request 'fixed flag cycle ?'                  'no'       fixflag
^request 'flagging cycle :'                     '<flagcycle>' flagcycle


^loop-marker auto1

^assign flag5D yes
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


^if %file_exists (<sd>/<RAScal>-<RASdate>/cal) ^system /usr/bin/rm <sd>/<RAScal>-<RASdate>/cal
^if %file_exists (<sd>/<RAScal>-<RASdate>/lsf) ^system /usr/bin/rm <sd>/<RAScal>-<RASdate>/lsf
^if %file_exists (<sd>/<RAStar>-<RASdate>/cal) ^system /usr/bin/rm <sd>/<RAStar>-<RASdate>/cal
^if %file_exists (<sd>/<RAStar>-<RASdate>/lsf) ^system /usr/bin/rm <sd>/<RAStar>-<RASdate>/lsf


cc create <RAScal> CAL file ...
calibration
update read <cal_RAS>
<aelist>
apply  [<cal_RAS>]  ae <aelist>

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



cc look at the rain gauge data
monitor
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
^request  'ae pair to plot : '    '1/5' plotlist
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
save cal: [<cal_RAS>] ae <aelist> sm <smooth> <smooth> flag 0,1

cc phase self-cal for <RAScal> ...
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
! cc and scan the merged data for <RAScal> ....
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

set-sample-file <RAStar>-<RASdate>
cc flag the data for the <RAStar> ...
^if %match (<autoflag>, no) ^jump noautofl

monitor
flag,<errorgate>,,
<aelist>
1,,,
yes no

^loop-marker noautofl
^if %match (<fixflag>, no) ^jump nofixflag
lsf
flag add 0
ae <aelist>
1(<flagcycle>)6000
set


^loop-marker nofixflag

^if  %match (<shadow>, no) ^jump noshad4
monitor
shadow
yes
^loop-marker noshad4



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


cc now make calibration for <RAStar> ... only reporting closure errors > 25 deg
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
^quit
