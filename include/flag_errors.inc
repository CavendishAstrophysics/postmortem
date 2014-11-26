C
C Error definitions for the FLAGLIB library
C
       integer    ill_flgexist
       parameter (ill_flgexist=-17101)! Existing table, cannot overwrite
       integer    ill_flgopen
       parameter (ill_flgopen=-17102) ! Flag table not open
       integer    ill_flgentry
       parameter (ill_flgentry=-17103) ! Illegal entry in flag table
       integer    ill_floption
       parameter (ill_floption=-17104) ! Illegal option requested
       integer    ill_flgdata
       parameter (ill_flgdata=-17105) ! Illegal data for flag entry
       integer    ill_flgunit
       parameter (ill_flgunit=-17106) ! Too many open units
