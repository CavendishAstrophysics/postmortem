c
c  Block data initialisation for the POSTMORTEM program
c
       block data
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/post_common.inc'
       include '/mrao/post/include/lsf_runtime.inc'
c
c  Current control tables logical unit number
       data   ct_lun  / 0 /

c  Current ionospheric correction tables logical unit number
       data   ion_lun / 0 /

c  Current logical sample file number
       data   curr_lsf_num / 0 /

c  Current root sample file name
       data   sf_name / ' ' /

c  Current default map name
       data   def_map / ' ' /

c  Initialisation flags for each subsystem
       data   cal_init / .false. /
       data   ion_init / .false. /
       data   map_init / .false. /
       data   rem_init / .false. /

       end
