cc calibrate multi-5-point offsets
cc  default ae 1-5

! 24 Aug 1998; 27 Oct 98; 16 Feb 99; 12 March 1999, 16 March 2000
cc 4 Sept 2003

^get variable sd SAMPDIR

^loop-marker START
^request 'date of observations, as YYMMDD : '  '<date>'  date
^request 'calibration number : '               '<calno>' calno
^request 'aerial list : '                      '1-5'     aelist
^request 'reference aerial : '                 '<refant>' refant
^request 'flag out shadowed data?'             'yes'     shadow
^assign flag6d no
^assign flag5D No
^assign flagE  No
^if %match (<aelist>, 1-6)                ^assign flag6d yes
^request 'flag out 6D ?'                       '<flag6d>' flag6d
^request 'flag out 5D?'                        '<flag5D>' flag5D
^request 'flag out sb E?'                      '<flagE>'  flagE

^loop-marker NEXT
^request 'name of source : '            '<source>' source


^if %file_exists(<sd>/<source>-<date>) ^jump SSF1

cc <sd>/<source>-<date> does not exist ...
^jump START

^loop-marker SSF1
cc delete any previous calibration and lsf, but not flags
^if %file_exists (<sd>/<source>-<date>/cal) ^system /usr/bin/rm <sd>/<source>-<date>/cal
^if %file_exists (<sd>/<source>-<date>/lsf) ^system /usr/bin/rm <sd>/<source>-<date>/lsf


cc create <source> CAL file ...
set-sample-file <source>-<date>
calibration
update read <calno>
<aelist>
apply  [<calno>]  ae <aelist>

cc flag the data ...

monitor
^if  %match (<shadow>, yes) ^jump s1
^jump s2
^loop-marker s1
shadow
yes
^loop-marker s2

cc auto flagging for ae 6 switched off
flag,,,
1-5
1,,,
yes no

^if  %match (<flag6d>, No) ^jump F1
lsf
flag add 0
ae 6; sb D
1-6000
set
-

^loop-marker F1
^if  %match (<flag5D>, No) ^jump F5d1
lsf
flag add 0
ae 5; sb D
1-6000
set
-
^loop-marker F5d1

^if  %match (<flagE>, No) ^jump FE1
lsf
flag add 0
sb E
1-6000
set
-
^loop-marker FE1

set-display amp-phase
^request  'plot one spacing of the data?' 'Yes' calplot
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
^request 'smoothing for calibration (samples) :' '25'      smooth

cc save calibration lsf ... note flag 0 only
lsf
smooth <smooth> <smooth>
select-flag 0
select-cal
yes
spacing ae <aelist>/<aelist>
save cal: [<calno>] ae <aelist> sm <smooth> <smooth> flag 0

cc phase self-cal for <source> ...
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
cc and plot one merged merged spacing for <source> [flag 0,1] ...
^request 'plot for aerial :' '<aerial>' aerial
lsf
sel-cal no yes
sel-flag 0,1
smooth 1 1
spacing ae <aerial>/<refant>
merge
sub-band
ae <aerial>/<refant>
1,,,



spacing ae <aelist>/<aelist>

smooth 5 5

monitor
cc  now set up for analyse-pointing ...


