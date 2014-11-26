
^get variable sd SAMPDIR
cc Sample-file diretory is <sd>
^assign date 990317


^loop-marker START
^request 'name of source : '                   '<source>' source
^request 'date of observations, as YYMMDD : '  '<date>'   date
^if %file_exists (<sd>/<source>-<date>) ^jump CONTINUE
cc cannot find <sd>/<source>-<date>
^jump START

^loop-marker CONTINUE
set-sample-file <source>-<date>

cal
new-geometry
yes
/mrao/post/util/geom_file.data
lsf
open
yes
select-cal
no
yes
save gfits
print-lsf,,


delete-lsf 1
yes
list-lsf

^loop-marker TAIL
^assign more no


^loop-marker MOREFIELDS
^request 'more fields ?', 'no', more
^if %match (<more>, YES) ^jump START

Q
^quit
