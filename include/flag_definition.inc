C
C Definition of entries in the flag table
C flg_list_name(30) added 8 Feb 93 [GP]
C
C Record length
       integer          flg_recl
       parameter       (flg_recl = 256)
       integer          flg_length
       parameter       (flg_length = flg_recl/4)
C
C Header record
       integer          flg_head_rec(flg_length)
C  flag file version
       integer          flg_head_version
       equivalence     (flg_head_version,flg_head_rec(1))
C  file name of the parent flag table file
       character*64     flg_head_file_name
       equivalence     (flg_head_file_name,flg_head_rec(2))
C  time ane date this file was created
       integer          flg_head_create_time(3)
       integer          flg_head_create_date(3)
       equivalence     (flg_head_create_time,flg_head_rec(18))
       equivalence     (flg_head_create_date,flg_head_rec(21))
C  number of entries in the file
       integer          flg_entries
       equivalence     (flg_entries,flg_head_rec(24))
C  last flag table entry version number in this file
       integer          flg_last_version
       equivalence     (flg_last_version,flg_head_rec(25))
C
C Standard record
       integer          flg_record(flg_length)
C
C  version number
       integer          flg_version
       equivalence     (flg_version,flg_record(1))
C  updated time/date
       integer          flg_timdat(6)
       equivalence     (flg_timdat(1),flg_record(2))
C  "updated by" entry
       character*32     flg_updated
       equivalence     (flg_updated,flg_record(8))
C  remainder of the record
       character*192    flg_string
       equivalence     (flg_string,flg_record(17))

C standard separator
       character*1      flg_char
       parameter       (flg_char = '%')

Control information
      integer           flg_max_unit
      parameter        (flg_max_unit = 30)
C  lists of open unit numbers and names for flag file(s)
       logical          flg_list_open(flg_max_unit)
       character*64     flg_list_name(flg_max_unit)
C  flag indicating re-calculation of the flag table array
       logical          flg_calculation
C  key specifying the LSF for which flag table array is calculated
       integer          flg_LSF
C  LSF and SF identification for flag table under construction
       integer          flg_lsf_num, flg_sf_lun
C  place control data in common
       common /flg_cntr/flg_list_open, flg_list_name,
     *                  flg_calculation, flg_LSF,
     *                  flg_lsf_num, flg_sf_lun


