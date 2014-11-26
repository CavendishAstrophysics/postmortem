cc calibrate with interleaved cal source - default ae 1-5
cc optional automatic flagging for target source only (rasters)
cc assumes a third, dummy, pointing centre has been used
cc uses sub-directory for maps and uses next-sample-file to get calibration SF

! 18 Feb 97...; 27 Oct 98 [unix verson]
cc 10 May 2006

^assign sampdir /data/ryle
^assign sd      /data/ryle
^assign md      ~/images
! cc sample-file directory is <sampdir>, map directory is <md>
^assign flagcycle  4

^loop-marker START
^request 'name of target source : '            '<RAStar>' RAStar
! ^request 'name of cal    source : '            '<RAScal>' RAScal
^request 'date of observations [eg 040105] : ' '<date!040105>' date

^if %file_exists(<sd>/<RAStar>-<date>) ^jump SSF1

cc <sd>/<RAStar>-<date> does not exist ...
^jump START

^loop-marker SSF1

! ^if %file_exists(<sd>/<RAScal>-<date>) ^jump SSF2

! cc <sd>/<RAScal>-<date> does not exist ...
! ^jump START

^loop-marker SSF2

! set up map sub-dir if needed ...
cc
^if %file_exists(<md>/<RAStar>) ^jump MDEXIST
@mkdir <md>/<RAStar>
cc <md>/<RAStar> directory created
^jump MDdone
^loop-marker MDEXIST
cc <md>/<RAStar> already exists
^loop-marker MDdone

set-sample-file <RAStar>-<date>

monitor
quit

cc call next-sample-file ...
next-sample-file
cc next-sample-file done
^file open /tmp/RT-sample-file.list READ unit
cc ^file open /tmp/RT-sample-file.list READ unit done
^file read  <unit> fullname
cc ^file read  <unit> fullname done
^file read  <unit> RAScal
cc ^file read  <unit> RAScal done
^file close <unit>
cc ^file close <unit> done

cc calibration with <fullname>

^request 'delete earlier flag files?' 'no', delflag
^if %match (<delflag>, no) ^jump nodelflag1
^if %file_exists (<sd>/<RAScal>-<date>/flag) ^system /usr/bin/rm <sd>/<RAScal>-<date>/flag
^if %file_exists (<sd>/<RAStar>-<date>/flag) ^system /usr/bin/rm <sd>/<RAStar>-<date>/flag

^loop-marker nodelflag1

^request 'calibration number : '               '<calno>'  calno
^request 'aerial list : '                      '1-5'      aelist
^request 'reference aerial : '                 <refant!1> refant
^request 'flag out shadowed data?'             'yes'      shadow
^request 'flag by pointing error for target ?' 'yes'      autoflag
^if %match (<autoflag>, yes) ^request 'error gate :' '100'  errorgate

^request 'fixed flag cycle ?'                  'yes'      fixflag
^if %match (<fixflag>, no) ^jump auto1

^loop-marker flagloop
^assign flagcycle 0
^if %match (P, <RAStar>) ^assign flagcycle 12
^if %match (S, <RAStar>) ^assign flagcycle 12
^if %match (R, <RAStar>) ^assign flagcycle  4

^request 'flagging cycle :'                     '<flagcycle>' flagcycle
^if %eq (<flagcycle>, 0) ^assign fixflag no

^loop-marker auto1

^assign flag5C No
^assign flag5D Yes
^assign flagE  No
^assign flag6D No

! ^request 'flag out 5C?'                        '<flag5C!no>'   flag5C
^request 'flag out 5D?'                        '<flag5D!no>'   flag5D
! ^request 'flag out sb E?'                      '<flagE!no>'    flagE
! ^request 'flag out 6D?'                        '<flag6D!no>'   flag6D

^assign   gen_flag 'no'
^deassign gen_list
^request 'flag out specified spacings?'        '<gen_flag!No>'  gen_flag
^if %match (<gen_flag>, no) ^jump NO_gen_flag
^request  'list of spacings - enclose in single quotes:' '<gen_list>' gen_list
^loop-marker NO_gen_flag

! get back to top level ... else attempt to delete cal/lsf may fail
Monitor
Q

cc delete any previous calibration and lsf


^if %file_exists (<sd>/<RAScal>-<date>/cal) ^system /usr/bin/rm <sd>/<RAScal>-<date>/cal
^if %file_exists (<sd>/<RAScal>-<date>/lsf) ^system /usr/bin/rm <sd>/<RAScal>-<date>/lsf
^if %file_exists (<sd>/<RAStar>-<date>/cal) ^system /usr/bin/rm <sd>/<RAStar>-<date>/cal
^if %file_exists (<sd>/<RAStar>-<date>/lsf) ^system /usr/bin/rm <sd>/<RAStar>-<date>/lsf


^loop-marker nodelflag1


cc create <RAScal> CAL file ...
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


^if  %match (<flag_5D>, no) ^jump no_flag_5D_1
lsf
flag add 0                                                             
ae 5 sb D                                                          
1-6000                                                                
set                                                                    
-

^loop-marker no_flag_5D_1



^if  %match (<gen_flag>, no) ^jump no_gen_flag_1
lsf
flag add 0
<gen_list>
1-6000
set
-

^loop-marker no_gen_flag_1


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
^request 'smoothing for cal source (samples) :' '20'      smooth

cc save calibration lsf ...
lsf
smooth <smooth> <smooth>
select-flag 0,1
select-cal
yes
save cal: [<calno>] ae <aelist> sm <smooth> <smooth> flag 0,1

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

^request 'archive flux-density of cal source ?' 'yes' arc
^if %match (<arc>, no) ^jump noarc
archive-flux
all
1,,,

^loop-marker noarc

set-sample-file <RAStar>-<date>
cc flag the data for the <RAStar> ...
^if %match (<autoflag>, no) ^jump noautofl

monitor
flag,<errorgate>,,
<aelist>
1,,,
yes no

^loop-marker noautofl
^if %match (<fixflag>, no)   ^jump nofixflag
^if %eq    (<flagcycle>, 0)  ^jump nofixflag
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

^if  %match (<flag_5D>, no) ^jump no_flag_5D_2  
lsf
flag add 0                                                             
ae 5 sb D                                                            
1-6000                                                                
set                                                                    
-

^loop-marker no_flag_5D_2


^if  %match (<gen_flag>, no) ^jump no_gen_flag_2
lsf
flag add 0
<gen_list>
1-6000
set
-
^loop-marker no_gen_flag_2



cc now make calibration for <RAStar> ... only reporting closure errors > 25 deg
calibration
solution check-closure 25
cal-file
no yes
yes

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

cc ... flag and cal files made; now use @hex-lsf <RAStar>-<date>
cc ... and run the resulting command file

^quit
